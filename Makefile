env:
	micromamba create -f env.yaml -y

clean:
	rm -rf artifacts

run:
	python3 -m ghc_hyperopt \
		--project-path FibHaskell \
		--component-name bench:bench-fib \
		--artifact-dir artifacts \

format:
	ruff format \
		--preview

check:
	ruff check \
		--preview \
		--fix

dashboard:
	optuna-dashboard sqlite:///artifacts/ghc_hyperopt.db \
		--server gunicorn \
		--artifact-dir artifacts

typecheck:
	pyright .