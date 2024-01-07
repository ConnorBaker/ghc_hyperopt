from dataclasses import dataclass
from typing import Self, override


@dataclass(frozen=True, unsafe_hash=True, order=True, slots=True)
class Version:
    """A three-digit version number."""

    major: int
    """The major version number."""

    minor: int
    """The minor version number."""

    patch: int
    """The patch version number."""

    @classmethod
    def parse(cls: type[Self], version: str) -> Self:
        """Parse a version number."""

        return cls(*map(int, version.split(".")))

    @override
    def __str__(self) -> str:
        """Convert the version number to a string."""

        return f"{self.major}.{self.minor}.{self.patch}"
