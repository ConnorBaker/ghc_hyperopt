from typing import ClassVar, final

from optuna import Trial

from ghc_hyperopt.ghc.option import GhcOption
from ghc_hyperopt.ghc.option_group import GhcOptionGroup
from ghc_hyperopt.utils import OurBaseModel, SampleFn, suggest_bool


@final
class GhcQuestionableOptionsOptunaSampler(OurBaseModel):
    """
    Sampler for questionable GHC options.
    """

    STATIC_ARGUMENT_TRANSFORMATION: ClassVar[SampleFn[Trial, bool]] = suggest_bool
    POLYMORPHIC_SPECIALISATION: ClassVar[SampleFn[Trial, bool]] = suggest_bool
    REGS_GRAPH: ClassVar[SampleFn[Trial, bool]] = suggest_bool
    REGS_ITERATIVE: ClassVar[SampleFn[Trial, bool]] = suggest_bool


@final
class GhcQuestionableOptions(GhcOptionGroup):
    """
    Tune questionable GHC compilation options. These options are questionable because they have
    historically caused problems or may break things or give bogus benchmark results.
    """

    # TODO: Tasty-Bench says that this value must always be false, as it is used to allow benchmarking.
    STATIC_ARGUMENT_TRANSFORMATION: ClassVar[GhcOption[bool]] = GhcOption(
        flag="static-argument-transformation",
        flag_prefix="-f",
        type=bool,
        default=False,
        kind="boolean",
        description=(
            """
            Turn on the static argument transformation, which turns a recursive function into a non-recursive one with
            a local recursive loop. See Chapter 7 of Andre Santos's PhD thesis.

            NOTE: This flag is known to cause problems. See the following quote from the Tasty-Bench documentation:

            > Never compile benchmarks with -fstatic-argument-transformation, because it breaks a trick we use to force
            > GHC into reevaluation of the same function application over and over again.
            > - https://github.com/Bodigrim/tasty-bench#troubleshooting
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fstatic-argument-transformation",
    )
    POLYMORPHIC_SPECIALISATION: ClassVar[GhcOption[bool]] = GhcOption(
        flag="polymorphic-specialisation",
        flag_prefix="-f",
        type=bool,
        default=False,
        kind="boolean",
        description=(
            """
            Warning, this feature is highly experimental and may lead to incorrect runtime results. Use at your own
            risk (#23469, #23109, #21229, #23445).

            Enable specialisation of function calls to known dictionaries with free type variables. The created
            specialisation will abstract over the type variables free in the dictionary.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fpolymorphic-specialisation",
    )
    REGS_GRAPH: ClassVar[GhcOption[bool]] = GhcOption(
        flag="regs-graph",
        flag_prefix="-f",
        type=bool,
        default=False,
        kind="boolean",
        description=(
            """
            Only applies in combination with the native code generator. Use the graph colouring register allocator for
            register allocation in the native code generator. By default, GHC uses a simpler, faster linear register
            allocator. The downside being that the linear register allocator usually generates worse code.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fregs-graph",
    )
    # TODO: Must be able to express dependency on REGS_GRAPH.
    REGS_ITERATIVE: ClassVar[GhcOption[bool]] = GhcOption(
        flag="regs-iterative",
        flag_prefix="-f",
        type=bool,
        default=False,
        kind="boolean",
        description=(
            """
            Only applies in combination with the native code generator. Use the iterative coalescing graph colouring
            register allocator for register allocation in the native code generator. This is the same register
            allocator as the -fregs-graph one but also enables iterative coalescing during register allocation.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fregs-iterative",
    )
