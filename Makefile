env:
	micromamba create -f env.yaml -y

clean:
	rm -rf FibHaskell-artifacts vector-artifacts

tune-ghc-FibHaskell:
	python3 -m ghc_hyperopt \
		--project-path "${PWD}/FibHaskell" \
		--component-name bench:bench-fib \
		--artifact-dir "${PWD}/FibHaskell-artifacts" \
		--tune-ghc \
		--tune-ghc-all

tune-ghc-vector:
	python3 -m ghc_hyperopt \
		--project-path "${PWD}/vector/vector" \
		--component-name bench:algorithms \
		--artifact-dir "${PWD}/vector-artifacts" \
		--tune-ghc \
		--tune-ghc-all

format:
	ruff format \
		--preview

check:
	ruff check \
		--preview \
		--fix

dashboard-FibHaskell:
	optuna-dashboard sqlite:///FibHaskell-artifacts/ghc_hyperopt.db \
		--server gunicorn \
		--artifact-dir FibHaskell-artifacts

dashboard-vector:
	optuna-dashboard sqlite:///vector-artifacts/ghc_hyperopt.db \
		--server gunicorn \
		--artifact-dir vector-artifacts

typecheck:
	pyright --stats .