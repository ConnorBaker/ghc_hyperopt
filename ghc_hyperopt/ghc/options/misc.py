from typing import ClassVar, final

from optuna import Trial

from ghc_hyperopt.ghc.option import GhcOption
from ghc_hyperopt.ghc.option_group import GhcOptionGroup
from ghc_hyperopt.ghc.requirement import GhcVersionRequirement
from ghc_hyperopt.utils import OurBaseModel, SampleFn, suggest_bool, suggest_int
from ghc_hyperopt.version import Version


@final
class GhcMiscOptionsOptunaSampler(OurBaseModel):
    """
    Sampler for GHC miscellaneous options.
    """

    DICTS_CHEAP: ClassVar[SampleFn[Trial, bool]] = suggest_bool
    DO_CLEVER_ARG_ETA_EXPANSION: ClassVar[SampleFn[Trial, bool]] = suggest_bool
    EAGER_BLACKHOLING: ClassVar[SampleFn[Trial, bool]] = suggest_bool
    EXCESS_PRECISION: ClassVar[SampleFn[Trial, bool]] = suggest_bool
    LIBERATE_CASE_THRESHOLD: ClassVar[SampleFn[Trial, int]] = suggest_int(100, 10000, log=True)
    MAX_WORKER_ARGS: ClassVar[SampleFn[Trial, int]] = suggest_int(1, 100, log=True)
    PEDANTIC_BOTTOMS: ClassVar[SampleFn[Trial, bool]] = suggest_bool
    DMD_UNBOX_WIDTH: ClassVar[SampleFn[Trial, int]] = suggest_int(0, 32)


@final
class GhcMiscOptions(GhcOptionGroup):
    """
    Tune miscellaneous GHC compilation options.
    """

    DICTS_CHEAP: ClassVar[GhcOption[bool]] = GhcOption(
        flag="dicts-cheap",
        flag_prefix="-f",
        type=bool,
        default=False,
        kind="boolean",
        description="A very experimental flag that makes dictionary-valued expressions seem cheap to the optimiser.",
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fdicts-cheap",
    )
    DO_CLEVER_ARG_ETA_EXPANSION: ClassVar[GhcOption[bool]] = GhcOption(
        flag="do-clever-arg-eta-expansion",
        flag_prefix="-f",
        type=bool,
        default=False,
        ghc_requirements=[
            GhcVersionRequirement(ghc_min_version=Version(value=(9, 9, 1))),
        ],
        kind="boolean",
        description="Eta-expand arguments to increase their arity to avoid allocating unnecessary thunks for them.",
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fdo-clever-arg-eta-expansion",
    )
    EAGER_BLACKHOLING: ClassVar[GhcOption[bool]] = GhcOption(
        flag="eager-blackholing",
        flag_prefix="-f",
        type=bool,
        default=False,
        kind="boolean",
        description=(
            """
            Usually GHC black-holes a thunk only when it switches threads. This flag makes it do so as soon as the
            thunk is entered. See Haskell on a shared-memory multiprocessor.

            See Compile-time options for SMP parallelism for a discussion on its use.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--feager-blackholing",
    )
    EXCESS_PRECISION: ClassVar[GhcOption[bool]] = GhcOption(
        flag="excess-precision",
        flag_prefix="-f",
        type=bool,
        default=False,
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
    )
    LIBERATE_CASE_THRESHOLD: ClassVar[GhcOption[int]] = GhcOption(
        flag="liberate-case-threshold",
        flag_prefix="-f",
        type=int,
        default=2000,
        kind="arg",
        description="Set the size threshold for the liberate-case transformation.",
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fliberate-case-threshold=⟨n⟩",
    )
    MAX_WORKER_ARGS: ClassVar[GhcOption[int]] = GhcOption(
        flag="max-worker-args",
        flag_prefix="-f",
        type=int,
        default=10,
        kind="arg",
        description=(
            """
            A function will not be split into worker and wrapper if the number of value arguments of the resulting
            worker exceeds both that of the original function and this setting.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fmax-worker-args=⟨n⟩",
    )
    PEDANTIC_BOTTOMS: ClassVar[GhcOption[bool]] = GhcOption(
        flag="pedantic-bottoms",
        flag_prefix="-f",
        type=bool,
        default=False,
        kind="boolean",
        description=(
            """
            Make GHC be more precise about its treatment of bottom (but see also -fno-state-hack). In particular, stop
            GHC eta-expanding through a case expression, which is good for performance, but bad if you are using seq
            on partial applications.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fpedantic-bottoms",
    )
    DMD_UNBOX_WIDTH: ClassVar[GhcOption[int]] = GhcOption(
        flag="dmd-unbox-width",
        flag_prefix="-f",
        type=int,
        default=3,
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
    )
