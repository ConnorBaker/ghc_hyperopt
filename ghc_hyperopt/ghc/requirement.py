from collections.abc import Set

from pydantic import Field

from ghc_hyperopt.ghc.info import (
    Architecture,
    GhcInfo,
    OperatingSystem,
    _Architectures,  # pyright: ignore[reportPrivateUsage]
    _OperatingSystems,  # pyright: ignore[reportPrivateUsage]
)
from ghc_hyperopt.utils import OurBaseModel, get_logger
from ghc_hyperopt.version import Version

logger = get_logger(__name__)


class GhcVersionRequirement(OurBaseModel):
    """
    Represents a GHC version requirement.
    """

    ghc_min_version: None | Version = None
    """The minimum GHC version supported."""

    ghc_max_version: None | Version = None
    """The maximum GHC version supported."""

    def satisfied_by(self, ghc_info: GhcInfo) -> bool:
        """
        Check if the requirement is satisfied by the given GHC version.

        :param ghc_info: The GHC info to check.
        :return: True if the requirement is satisfied, False otherwise.
        """
        logger.debug("Checking if %s satisfies %s", ghc_info, self)
        min_bound_sat = self.ghc_min_version is None or ghc_info.version >= self.ghc_min_version
        logger.debug("Min bound satisfied: %s", min_bound_sat)
        max_bound_sat = self.ghc_max_version is None or ghc_info.version <= self.ghc_max_version
        logger.debug("Max bound satisfied: %s", max_bound_sat)

        return min_bound_sat and max_bound_sat


class GhcArchitectureRequirement(OurBaseModel):
    """
    Represents a GHC architecture requirement.
    """

    architectures: Set[Architecture] = Field(default_factory=lambda: _Architectures)
    """The architectures supported."""

    def satisfied_by(self, ghc_info: GhcInfo) -> bool:
        """
        Check if the requirement is satisfied by the given architecture.

        :param ghc_info: The GHC info to check.
        :return: True if the requirement is satisfied, False otherwise.
        """
        return ghc_info.architecture in self.architectures


class GhcOperatingSystemRequirement(OurBaseModel):
    """
    Represents a GHC operating system requirement.
    """

    operating_systems: Set[OperatingSystem] = Field(default_factory=lambda: _OperatingSystems)
    """The operating systems supported."""

    def satisfied_by(self, ghc_info: GhcInfo) -> bool:
        """
        Check if the requirement is satisfied by the given operating system.

        :param ghc_info: The GHC info to check.
        :return: True if the requirement is satisfied, False otherwise.
        """
        return ghc_info.operating_system in self.operating_systems


type GhcRequirement = GhcVersionRequirement | GhcArchitectureRequirement | GhcOperatingSystemRequirement
