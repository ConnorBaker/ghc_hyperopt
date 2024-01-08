from argparse import ArgumentParser, _ArgumentGroup  # pyright: ignore[reportPrivateUsage]
from typing import Self

from ghc_hyperopt.utils import OurBaseModel


class GhcOptionGroup(OurBaseModel):
    """A group of related GHC options."""

    @classmethod
    def create_group_from_parser(cls: type[Self], arg_parser: ArgumentParser) -> _ArgumentGroup:
        """Create the group from the given argument parser."""
        # The name of the class is used as the title.
        title = cls.__name__
        assert title is not None

        # The docstring is used as the description.
        description = cls.__doc__
        assert description is not None

        return arg_parser.add_argument_group(
            title=title,
            description=description + "\n\nWhen a flag is not specified, the corresponding option is not tuned.",
        )

    @classmethod
    def add_options_to_group(cls: type[Self], arg_group: _ArgumentGroup) -> None:
        """Add the group to the given argument parser."""
        for name in cls.__class_vars__:
            _ = arg_group.add_argument(
                f"--tune-ghc-{name}",
                action="store_true",
                help=getattr(cls, name).description,
            )

    @classmethod
    def add_group_to_parser(cls: type[Self], arg_parser: ArgumentParser) -> None:
        """Add the group to the given argument parser."""
        cls.add_options_to_group(cls.create_group_from_parser(arg_parser))
