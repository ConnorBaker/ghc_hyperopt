import logging
import math
from collections.abc import Callable, Sequence
from typing import Any, Self, TypeAlias, TypeVar

from optuna import Trial
from pydantic import (
    BaseModel,
    ConfigDict,
    ValidationError,
    ValidatorFunctionWrapHandler,
    model_validator,
)


# NOTE: Parsing unknown fields amounts to dumping the values and parsing them again.
#       This is required as of Pydantic 2.5.3
# https://github.com/pydantic/pydantic/issues/8071#issuecomment-1808405904
class OurBaseModel(BaseModel):
    model_config = ConfigDict(
        extra="forbid",
        frozen=True,
        strict=True,
        # NOTE: We must enable both from_attributes and revalidate_instances so nested generic models work --
        #       essentially asking Pydantic to dump and reparse the values.
        #       This is in addition to the custom model validator below.
        from_attributes=True,
    )

    @model_validator(mode="wrap")
    @classmethod
    def dump_on_non_subclass(cls, values: Any, handler: ValidatorFunctionWrapHandler) -> Self:
        """
        This validator is used to revalidate the dumped data if you get a non-parametrized generic
        as an input to a parametrized input field of a type that is a subclass of `OurBaseModel`.
        """
        # The approach used here assumes that validation won't have side-effects, so it's safe to attempt to run twice.
        # If that is wrong, you should use the logic from the exception handler below prior to the first attempt to use
        # the handler.
        try:
            return handler(values)
        except ValidationError as exc:
            if not isinstance(values, BaseModel):
                # If a basemodel wasn't passed, validation didn't fail due to a non-parametrized generic instance
                raise

            if not any(e["type"] == "model_type" and e["loc"] == () for e in exc.errors()):
                # Only try to handle errors that are model_type errors
                # (Might need to tweak the condition a bit... but this worked/made sense to me)
                raise

            origin = cls.__pydantic_generic_metadata__.get("origin")  # the non-parametrized version of this class
            if origin is None:
                # This is just to handle the case where you accidentally create a subclass of this that is not generic
                # and attempt to instantiate it. You could drop this branch if you won't ever do that.
                raise
            if not isinstance(values, origin):
                # This means the input data was not an instance of a subclass of the non-parametrized version of this
                # class, so it probably makes sense to just raise the error. If you wanted to be even more lax, you
                # could drop this condition (and most of the logic related to generic origins, etc.) and it will
                # always attempt to revalidate the dumped data.
                raise

            # This assumes that validating dumped data is idempotent, but if it isn't, it's not clear to me what you'd
            # want to do here anyway. But you could change the logic as necessary for your use case.
            return handler(values.model_dump())


class OurBaseException(Exception):
    msg: str
    """The error message."""

    def __init__(self, msg: str) -> None:
        super().__init__(msg)
        self.msg = msg


Sampler = TypeVar("Sampler")
A = TypeVar("A")

SampleFn: TypeAlias = Callable[[str, Sampler], A]


def suggest_int(low: int, high: int, log: bool = False) -> SampleFn[Trial, int]:
    return lambda name, trial: trial.suggest_int(name=name, low=low, high=high, log=log)


def suggest_from(choices: Sequence[A]) -> SampleFn[Trial, A]:
    return lambda name, trial: trial.suggest_categorical(name=name, choices=choices)  # type: ignore


suggest_bool: SampleFn[Trial, bool] = suggest_from([False, True])


def get_logger(name: str) -> logging.Logger:
    logger = logging.getLogger(name)
    logger.setLevel(logging.DEBUG)
    handler = logging.StreamHandler()
    handler.setLevel(logging.DEBUG)
    handler.setFormatter(logging.Formatter("%(asctime)s %(name)s %(levelname)s %(message)s"))
    logger.addHandler(handler)
    return logger


def percent_improvement(new: float, old: float) -> float:
    denom = math.copysign(1e-6, old) if math.isclose(old, 0) else old
    return (new - old) / denom * 100.0
