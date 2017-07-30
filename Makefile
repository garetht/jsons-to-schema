.PHONY: test
test: ## runs both pep8 and pyflakes
	JSON_ENVIRONMENT=local stack test --resolver=lts-9 && stack test --resolver=lts-8 && stack test --resolver=lts-6
