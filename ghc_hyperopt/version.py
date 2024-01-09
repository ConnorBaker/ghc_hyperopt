from typing import Self

from typing_extensions import override

from ghc_hyperopt.utils import OurBaseModel


class Version(OurBaseModel):
    """A three-digit version number."""

    value: tuple[int, int, int]

    def __lt__(self, other: Self) -> bool:
        """Compare two version numbers."""

        return self.value < other.value

    def __le__(self, other: Self) -> bool:
        """Compare two version numbers."""

        return self.value <= other.value

    def __gt__(self, other: Self) -> bool:
        """Compare two version numbers."""

        return self.value > other.value

    def __ge__(self, other: Self) -> bool:
        """Compare two version numbers."""

        return self.value >= other.value

    @classmethod
    def parse(cls: type[Self], version: str) -> Self:
        """Parse a version number."""

        (major, minor, patch) = map(int, version.split("."))
        return cls(value=(major, minor, patch))

    @override
    def __str__(self) -> str:
        """Convert the version number to a string."""

        return ".".join(map(str, self.value))
