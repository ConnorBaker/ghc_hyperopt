from dataclasses import dataclass

from ghc_hyperopt.ghc.option import GhcOption, GhcOptionGroup
from ghc_hyperopt.ghc.requirement import GhcVersionRequirement
from ghc_hyperopt.utils import suggest_bool, suggest_int
from ghc_hyperopt.version import Version


@dataclass(frozen=True, unsafe_hash=True, slots=True, kw_only=True)
class _GhcMiscOptions(GhcOptionGroup):
    title = "GHC miscellaneous tuning"
    description = """
        Tune miscellaneous GHC compilation options.

        When a flag is not specified, the corresponding option is not tuned.
        """

    DICTS_CHEAP: GhcOption[bool]
    DO_CLEVER_ARG_ETA_EXPANSION: GhcOption[bool]
    EAGER_BLACKHOLING: GhcOption[bool]
    EXCESS_PRECISION: GhcOption[bool]
    LIBERATE_CASE_THRESHOLD: GhcOption[int]
    MAX_WORKER_ARGS: GhcOption[int]
    PEDANTIC_BOTTOMS: GhcOption[bool]
    DMD_UNBOX_WIDTH: GhcOption[int]


GhcMiscOptions = _GhcMiscOptions(
    DICTS_CHEAP=GhcOption(
        flag="dicts-cheap",
        flag_prefix="-f",
        type=bool,
        default=False,
        sample=suggest_bool,
        kind="boolean",
        description="A very experimental flag that makes dictionary-valued expressions seem cheap to the optimiser.",
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fdicts-cheap",
    ),
    DO_CLEVER_ARG_ETA_EXPANSION=GhcOption(
        flag="do-clever-arg-eta-expansion",
        flag_prefix="-f",
        type=bool,
        default=False,
        ghc_requirements=[
            GhcVersionRequirement(ghc_min_version=Version(9, 9, 1)),
        ],
        sample=suggest_bool,
        kind="boolean",
        description="Eta-expand arguments to increase their arity to avoid allocating unnecessary thunks for them.",
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fdo-clever-arg-eta-expansion",
    ),
    EAGER_BLACKHOLING=GhcOption(
        flag="eager-blackholing",
        flag_prefix="-f",
        type=bool,
        default=False,
        sample=suggest_bool,
        kind="boolean",
        description=(
            """
            Usually GHC black-holes a thunk only when it switches threads. This flag makes it do so as soon as the
            thunk is entered. See Haskell on a shared-memory multiprocessor.

            See Compile-time options for SMP parallelism for a discussion on its use.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--feager-blackholing",
    ),
    EXCESS_PRECISION=GhcOption(
        flag="excess-precision",
        flag_prefix="-f",
        type=bool,
        default=False,
        sample=suggest_bool,
        kind="boolean",
        description=(
            """
            When this option is given, intermediate floating point values can have a greater precision/range than the
            final type. Generally this is a good thing, but some programs may rely on the exact precision/range of
            Float/Double values and should not use this option for their compilation.

            Note that the 32-bit x86 native code generator only supports excess-precision mode, so neither
            -fexcess-precision nor -fno-excess-precision has any effect. This is a known bug, see Bugs in GHC.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fexcess-precision",
    ),
    LIBERATE_CASE_THRESHOLD=GhcOption(
        flag="liberate-case-threshold",
        flag_prefix="-f",
        type=int,
        default=2000,
        sample=suggest_int(100, 10000, log=True),
        kind="arg",
        description="Set the size threshold for the liberate-case transformation.",
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fliberate-case-threshold=⟨n⟩",
    ),
    MAX_WORKER_ARGS=GhcOption(
        flag="max-worker-args",
        flag_prefix="-f",
        type=int,
        default=10,
        sample=suggest_int(1, 100, log=True),
        kind="arg",
        description=(
            """
            A function will not be split into worker and wrapper if the number of value arguments of the resulting
            worker exceeds both that of the original function and this setting.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fmax-worker-args=⟨n⟩",
    ),
    PEDANTIC_BOTTOMS=GhcOption(
        flag="pedantic-bottoms",
        flag_prefix="-f",
        type=bool,
        default=False,
        sample=suggest_bool,
        kind="boolean",
        description=(
            """
            Make GHC be more precise about its treatment of bottom (but see also -fno-state-hack). In particular, stop
            GHC eta-expanding through a case expression, which is good for performance, but bad if you are using seq
            on partial applications.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fpedantic-bottoms",
    ),
    DMD_UNBOX_WIDTH=GhcOption(
        flag="dmd-unbox-width",
        flag_prefix="-f",
        type=int,
        default=3,
        sample=suggest_int(0, 32),
        kind="arg",
        description=(
            """
            Boxity analysis optimistically pretends that a function returning a record with at most -fdmd-unbox-width
            fields has only call sites that don't need the box of the returned record. That may in turn allow more
            argument unboxing to happen. Set to 0 to be completely conservative (which guarantees that no reboxing will
            happen due to this mechanism).
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fdmd-unbox-width=⟨n⟩",
    ),
)
