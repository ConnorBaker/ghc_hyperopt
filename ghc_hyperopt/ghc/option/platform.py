from dataclasses import dataclass

from ghc_hyperopt.ghc.option import GhcOption, GhcOptionGroup
from ghc_hyperopt.ghc.requirement import GhcArchitectureRequirement, GhcVersionRequirement
from ghc_hyperopt.utils import suggest_bool
from ghc_hyperopt.version import Version


@dataclass(frozen=True, unsafe_hash=True, slots=True, kw_only=True)
class _GhcPlatformOptions(GhcOptionGroup):
    """
    Represents GHC options related to the platform.
    """

    title = "GHC platform tuning"
    description = """
        Tune GHC compilation options related to the platform.

        When a flag is not specified, the corresponding option is not tuned.
        """

    # Only including support for options for the next-gen code generator.
    SSE4_2: GhcOption[bool]
    BMI2: GhcOption[bool]
    FMA: GhcOption[bool]


GhcPlatformOptions = _GhcPlatformOptions(
    SSE4_2=GhcOption(
        flag="sse4.2",
        flag_prefix="-m",
        type=bool,
        default=False,
        sample=suggest_bool,
        kind="present_or_absent",
        ghc_requirements=[
            GhcVersionRequirement(ghc_min_version=Version(7, 4, 1)),
            GhcArchitectureRequirement({"x86_64"}),
        ],
        description=(
            """
            (x86 only, added in GHC 7.4.1) Use the SSE4.2 instruction set to implement some floating point and bit
            operations when using the native code generator. The resulting compiled code will only run on processors
            that support SSE4.2 (Intel Core i7 and later). The LLVM backend will also use SSE4.2 if your processor
            supports it but detects this automatically so no flag is required.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using.html#ghc-flag--msse4.2",
    ),
    BMI2=GhcOption(
        flag="bmi2",
        flag_prefix="-m",
        type=bool,
        default=False,
        sample=suggest_bool,
        kind="present_or_absent",
        ghc_requirements=[
            GhcVersionRequirement(ghc_min_version=Version(7, 4, 1)),
            GhcArchitectureRequirement({"x86_64"}),
        ],
        description=(
            """
            (x86 only, added in GHC 7.4.1) Use the BMI2 instruction set to implement some bit operations when using the
            native code generator. The resulting compiled code will only run on processors that support BMI2 (Intel
            Haswell and newer, AMD Excavator, Zen and newer).
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using.html#ghc-flag--mbmi2",
    ),
    FMA=GhcOption(
        flag="fma",
        flag_prefix="-m",
        type=bool,
        default=False,
        sample=suggest_bool,
        kind="present_or_absent",
        ghc_requirements=[
            GhcVersionRequirement(ghc_min_version=Version(9, 8, 1)),
        ],
        description=(
            """
            Use native FMA instructions to implement the fused multiply-add floating-point operations of the form x * y
            + z. This allows computing a multiplication and addition in a single instruction, without an intermediate
            rounding step. Supported architectures: X86 with the FMA3 instruction set (this includes most consumer
            processors since 2013), PowerPC and AArch64.

            When this flag is disabled, GHC falls back to the C implementation of fused multiply-add, which might
            perform non-IEEE-compliant software emulation on some platforms (depending on the implementation of the C
            standard library).
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using.html#ghc-flag--mfma",
    ),
)
