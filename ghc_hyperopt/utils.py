import logging
from collections.abc import Callable, Sequence

from optuna import Trial
from pydantic import BaseModel, ConfigDict


class OurBaseModel(BaseModel):
    model_config = ConfigDict(extra="forbid", frozen=True, strict=True)


class OurBaseException(Exception):
    msg: str
    """The error message."""

    def __init__(self, msg: str) -> None:
        super().__init__(msg)
        self.msg = msg


type SampleFn[Sampler, A] = Callable[[str, Sampler], A]


def suggest_int(low: int, high: int, log: bool = False) -> SampleFn[Trial, int]:
    return lambda name, trial: trial.suggest_int(name=name, low=low, high=high, log=log)


def suggest_from[T](choices: Sequence[T]) -> SampleFn[Trial, T]:
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
