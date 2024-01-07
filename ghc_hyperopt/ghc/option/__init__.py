from argparse import ArgumentParser, _ArgumentGroup  # pyright: ignore[reportPrivateUsage]
from collections.abc import Callable, Sequence
from dataclasses import dataclass, field
from typing import ClassVar, Literal, Protocol, Self, overload, runtime_checkable

from optuna import Trial

from ghc_hyperopt.ghc.requirement import GhcRequirement


@dataclass(frozen=True, unsafe_hash=True, slots=True, kw_only=True)
class GhcOption[T]:
    """
    Represents a GHC option.
    """

    flag: str
    """The flag to pass to GHC."""

    flag_prefix: str
    """The prefix to use for the flag."""

    default: T
    """The default value of the option."""

    type: type[T]
    """The value of the option."""

    # TODO: Make sample accept a mapping of all arguments selected so far so it can make a more informed decision.
    sample: Callable[[str, Trial], T]
    """A function which converts a sample to the option value."""

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

    ghc_requirements: Sequence[GhcRequirement] = field(default_factory=list)
    """GHC-specific requirements for the option to be considered."""

    def __post_init__(self) -> None:
        """Validate the option, raising an exception if it is invalid."""
        if self.kind == "present_or_absent":
            if self.type is not bool:
                raise TypeError(f"Option {self.flag} must be a boolean.")
        elif self.kind == "boolean":
            if self.type is not bool:
                raise TypeError(f"Option {self.flag} must be a boolean.")
        elif self.kind == "arg":
            if self.type is not int:
                raise TypeError(f"Option {self.flag} must be an int.")
        else:
            raise TypeError(f"Option {self.flag} has an invalid kind {self.kind}.")

    @overload
    def to_flag(
        self,
        use: Literal["default"],
        *,
        value: None = None,
        trial: None = None,
    ) -> None | str: ...

    @overload
    def to_flag(
        self,
        use: Literal["value"],
        *,
        value: T,
        trial: None = None,
    ) -> None | str: ...

    @overload
    def to_flag(
        self,
        use: Literal["trial"],
        *,
        value: None = None,
        trial: Trial,
    ) -> None | str: ...

    def to_flag(
        self,
        use: Literal["default", "value", "trial"],
        *,
        value: None | T = None,
        trial: None | Trial = None,
    ) -> None | str:
        """
        Convert the option to a flag.

        :param value: The value of the option
        :return: The flag, or None if the flag should not be passed.
        """
        match use:
            case "default":
                assert value is None
                assert trial is None
                value = self.default
            case "value":
                assert value is not None
                assert trial is None
            case "trial":
                assert value is None
                assert trial is not None
                value = self.sample(self.flag, trial)

        match self.kind:
            case "present_or_absent":
                return f"{self.flag_prefix}{self.flag}" if value else None
            case "boolean":
                return f"{self.flag_prefix}" + ("" if value else "no-") + f"{self.flag}"
            case "arg":
                return f"{self.flag_prefix}{self.flag}={value}"


@runtime_checkable
class GhcOptionGroup(Protocol):
    """A group of related GHC options."""

    title: str

    description: str

    __slots__: ClassVar[Sequence[str]] = []

    def create_group_from_parser(self: Self, arg_parser: ArgumentParser) -> _ArgumentGroup:
        """Create the group from the given argument parser."""
        return arg_parser.add_argument_group(
            title=self.title,
            description=self.description,
        )

    def add_options_to_group(self: Self, arg_group: _ArgumentGroup) -> None:
        """Add the group to the given argument parser."""
        ignored_fields = {"title", "description"}
        for name in filter(lambda name: name not in ignored_fields, self.__slots__):
            _ = arg_group.add_argument(
                f"--tune-ghc-{name}",
                action="store_true",
                help=getattr(self, name).description,
            )

    def add_group_to_parser(self: Self, arg_parser: ArgumentParser) -> None:
        """Add the group to the given argument parser."""
        self.add_options_to_group(self.create_group_from_parser(arg_parser))
