from typing import ClassVar, final

from optuna import Trial

from ghc_hyperopt.ghc.option import GhcOption
from ghc_hyperopt.ghc.option_group import GhcOptionGroup
from ghc_hyperopt.utils import OurBaseModel, SampleFn, suggest_bool, suggest_int


@final
class GhcSimplifierOptionsOptunaSampler(OurBaseModel):
    """
    Sampler for GHC simplifier options.
    """

    MAX_SIMPLIFIER_ITERATIONS: ClassVar[SampleFn[Trial, int]] = suggest_int(1, 100, log=True)
    SIMPLIFIER_PHASES: ClassVar[SampleFn[Trial, int]] = suggest_int(1, 100, log=True)
    SIMPL_TICK_FACTOR: ClassVar[SampleFn[Trial, int]] = suggest_int(1, 10000, log=True)
    LATE_DMD_ANAL: ClassVar[SampleFn[Trial, bool]] = suggest_bool


@final
class GhcSimplifierOptions(GhcOptionGroup):
    """
    Tune GHC compilation options related to the simplifier.
    """

    MAX_SIMPLIFIER_ITERATIONS: ClassVar[GhcOption[int]] = GhcOption(
        flag="max-simplifier-iterations",
        flag_prefix="-f",
        type=int,
        default=4,
        kind="arg",
        description="Sets the maximal number of iterations for the simplifier.",
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fmax-simplifier-iterations=⟨n⟩",
    )
    SIMPLIFIER_PHASES: ClassVar[GhcOption[int]] = GhcOption(
        flag="simplifier-phases",
        flag_prefix="-f",
        type=int,
        default=2,
        kind="arg",
        description="Set the number of phases for the simplifier.",
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fsimplifier-phases=⟨n⟩",
    )
    SIMPL_TICK_FACTOR: ClassVar[GhcOption[int]] = GhcOption(
        flag="simpl-tick-factor",
        flag_prefix="-f",
        type=int,
        default=100,
        kind="arg",
        description=(
            """
            GHC's optimiser can diverge if you write rewrite rules (Rewrite rules) that don't terminate, or (less
            satisfactorily) if you code up recursion through data types (Bugs in GHC). To avoid making the compiler
            fall into an infinite loop, the optimiser carries a “tick count” and stops inlining and applying rewrite
            rules when this count is exceeded. The limit is set as a multiple of the program size, so bigger programs
            get more ticks. The -fsimpl-tick-factor flag lets you change the multiplier. The default is 100; numbers
            larger than 100 give more ticks, and numbers smaller than 100 give fewer.

            If the tick-count expires, GHC summarises what simplifier steps it has done; you can use
            -fddump-simpl-stats to generate a much more detailed list. Usually that identifies the loop quite
            accurately, because some numbers are very large.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fsimpl-tick-factor=⟨n⟩",
    )
    LATE_DMD_ANAL: ClassVar[GhcOption[bool]] = GhcOption(
        flag="late-dmd-anal",
        flag_prefix="-f",
        type=bool,
        default=False,
        kind="boolean",
        description=(
            """
            Run demand analysis again, at the end of the simplification pipeline. We found some opportunities for
            discovering strictness that were not visible earlier; and optimisations like -fspec-constr can create
            functions with unused arguments which are eliminated by late demand analysis. Improvements are modest, but
            so is the cost. See notes on the wiki page.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--flate-dmd-anal",
    )
