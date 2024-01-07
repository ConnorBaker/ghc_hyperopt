from collections.abc import Set
from dataclasses import dataclass, field
from typing import Protocol, override, runtime_checkable

from ghc_hyperopt.ghc.info import (
    Architecture,
    GhcInfo,
    OperatingSystem,
    _Architectures,  # pyright: ignore[reportPrivateUsage]
    _OperatingSystems,  # pyright: ignore[reportPrivateUsage]
)
from ghc_hyperopt.version import Version


@runtime_checkable
class GhcRequirement(Protocol):
    """
    Represents a GHC requirement.
    """

    def satisfied_by(self, ghc_info: GhcInfo) -> bool:
        """
        Check if the requirement is satisfied by the given GHC info.

        :param ghc_info: The GHC info to check.
        :return: True if the requirement is satisfied, False otherwise.
        """
        ...


@dataclass(frozen=True, unsafe_hash=True, slots=True, kw_only=True)
class GhcVersionRequirement(GhcRequirement):
    """
    Represents a GHC version requirement.
    """

    ghc_min_version: None | Version = None
    """The minimum GHC version supported."""

    ghc_max_version: None | Version = None
    """The maximum GHC version supported."""

    @override
    def satisfied_by(self, ghc_info: GhcInfo) -> bool:
        """
        Check if the requirement is satisfied by the given GHC version.

        :param ghc_info: The GHC info to check.
        :return: True if the requirement is satisfied, False otherwise.
        """
        return all([
            self.ghc_min_version is None or ghc_info.version >= self.ghc_min_version,
            self.ghc_max_version is None or ghc_info.version <= self.ghc_max_version,
        ])


@dataclass(frozen=True, unsafe_hash=True, slots=True)
class GhcArchitectureRequirement(GhcRequirement):
    """
    Represents a GHC architecture requirement.
    """

    architectures: Set[Architecture] = field(default_factory=lambda: _Architectures)
    """The architectures supported."""

    @override
    def satisfied_by(self, ghc_info: GhcInfo) -> bool:
        """
        Check if the requirement is satisfied by the given architecture.

        :param ghc_info: The GHC info to check.
        :return: True if the requirement is satisfied, False otherwise.
        """
        return ghc_info.architecture in self.architectures


@dataclass(frozen=True, unsafe_hash=True, slots=True)
class GhcOperatingSystemRequirement(GhcRequirement):
    """
    Represents a GHC operating system requirement.
    """

    operating_systems: Set[OperatingSystem] = field(default_factory=lambda: _OperatingSystems)
    """The operating systems supported."""

    @override
    def satisfied_by(self, ghc_info: GhcInfo) -> bool:
        """
        Check if the requirement is satisfied by the given operating system.

        :param ghc_info: The GHC info to check.
        :return: True if the requirement is satisfied, False otherwise.
        """
        return ghc_info.operating_system in self.operating_systems
