from typing import ClassVar, final

from optuna import Trial

from ghc_hyperopt.ghc.option import GhcOption
from ghc_hyperopt.ghc.option_group import GhcOptionGroup
from ghc_hyperopt.utils import OurBaseModel, SampleFn, suggest_bool, suggest_int


@final
class GhcInlinerOptionsOptunaSampler(OurBaseModel):
    """
    Sampler for GHC inliner options.
    """

    MAX_INLINE_ALLOC_SIZE: ClassVar[SampleFn[Trial, int]] = suggest_int(32, 4096, log=True)
    MAX_INLINE_MEMCPY_INSNS: ClassVar[SampleFn[Trial, int]] = suggest_int(8, 4096, log=True)
    MAX_INLINE_MEMSET_INSNS: ClassVar[SampleFn[Trial, int]] = suggest_int(8, 4096, log=True)
    EXPOSE_ALL_UNFOLDINGS: ClassVar[SampleFn[Trial, bool]] = suggest_bool


@final
class GhcInlinerOptions(GhcOptionGroup, OurBaseModel):
    """
    Tune GHC compilation options related to the inliner.
    """

    # TODO: Need to be able to encode "This value should be quite a bit smaller than the block size (typically: 4096)."
    MAX_INLINE_ALLOC_SIZE: ClassVar[GhcOption[int]] = GhcOption(
        flag="max-inline-alloc-size",
        flag_prefix="-f",
        type=int,
        default=128,
        kind="arg",
        description=(
            """
            Set the maximum size of inline array allocations to n bytes. GHC will allocate non-pinned arrays of
            statically known size in the current nursery block if they're no bigger than n bytes, ignoring GC
            overheap. This value should be quite a bit smaller than the block size (typically: 4096).
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fmax-inline-alloc-size=⟨n⟩",
    )
    MAX_INLINE_MEMCPY_INSNS: ClassVar[GhcOption[int]] = GhcOption(
        flag="max-inline-memcpy-insns",
        flag_prefix="-f",
        type=int,
        default=32,
        kind="arg",
        description="Inline memcpy calls if they would generate no more than ⟨n⟩ pseudo-instructions.",
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fmax-inline-memcpy-insns=⟨n⟩",
    )
    MAX_INLINE_MEMSET_INSNS: ClassVar[GhcOption[int]] = GhcOption(
        flag="max-inline-memset-insns",
        flag_prefix="-f",
        type=int,
        default=32,
        kind="arg",
        description="Inline memset calls if they would generate no more than n pseudo instructions.",
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fmax-inline-memset-insns=⟨n⟩",
    )
    EXPOSE_ALL_UNFOLDINGS: ClassVar[GhcOption[bool]] = GhcOption(
        flag="expose-all-unfoldings",
        flag_prefix="-f",
        type=bool,
        default=False,
        kind="boolean",
        description=(
            """
            An experimental flag to expose all unfoldings, even for very large or recursive functions. This allows for
            all functions to be inlined while usually GHC would avoid inlining larger functions.

            NOTE: Also see the -fspecialise-aggressively flag.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fexpose-all-unfoldings",
    )
