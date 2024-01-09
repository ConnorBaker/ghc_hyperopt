from collections.abc import Sequence
from typing import Generic, Literal, Self, TypeVar, final, overload

from pydantic import Field, model_validator

from ghc_hyperopt.ghc.requirement import GhcRequirement
from ghc_hyperopt.utils import OurBaseModel

A = TypeVar("A")


@final
class GhcOption(OurBaseModel, Generic[A]):
    """
    Represents a GHC option.
    """

    flag: str
    """The flag to pass to GHC."""

    flag_prefix: str
    """The prefix to use for the flag."""

    default: A
    """The default value of the option."""

    type: type[A]
    """The value of the option."""

    kind: Literal["present_or_absent", "boolean", "arg"]
    """
    The kind of the option.

    Flags which are "present_or_absent" must be booleans and are passed as <flag_prefix><flag> if True or not at all
    if False.

    Flags which are "boolean" must be booleans and are passed as <flag_prefix><flag> if True or <flag_prefix>no-<flag>
    if False.

    Flags which are "arg" must be an int and are passed as <flag_prefix><flag>=<value>.
    """

    description: str
    """A description of the option."""

    reference: str
    """A reference to the documentation for the option."""

    ghc_requirements: Sequence[GhcRequirement] = Field(default_factory=list)
    """GHC-specific requirements for the option to be considered."""

    @model_validator(mode="after")
    def check_kind_and_type_are_consistent(self: Self) -> Self:
        """Validate the option, raising an exception if it is invalid."""
        if self.kind == "present_or_absent":
            if self.type is not bool:
                raise ValueError(f"Option {self.flag} must be a boolean.")
            return self
        elif self.kind == "boolean":
            if self.type is not bool:
                raise ValueError(f"Option {self.flag} must be a boolean.")
            return self
        elif self.kind == "arg":
            if self.type is not int:
                raise ValueError(f"Option {self.flag} must be an int.")
            return self
        else:
            raise ValueError(f"Option {self.flag} has an invalid kind {self.kind}.")

    @overload
    def to_flag(
        self,
        use: Literal["default"],
        *,
        value: None = None,
    ) -> None | str: ...

    @overload
    def to_flag(
        self,
        use: Literal["value"],
        *,
        value: A,
    ) -> None | str: ...

    def to_flag(
        self,
        use: Literal["default", "value"],
        *,
        value: None | A = None,
    ) -> None | str:
        """
        Convert the option to a flag.

        :param value: The value of the option
        :return: The flag, or None if the flag should not be passed.
        """
        match use:
            case "default":
                assert value is None
                value = self.default
            case "value":
                assert value is not None

        match self.kind:
            case "present_or_absent":
                return f"{self.flag_prefix}{self.flag}" if value else None
            case "boolean":
                return f"{self.flag_prefix}" + ("" if value else "no-") + f"{self.flag}"
            case "arg":
                return f"{self.flag_prefix}{self.flag}={value}"
