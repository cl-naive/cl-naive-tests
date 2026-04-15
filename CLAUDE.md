# cl-naive-tests

See `~/.claude/CLAUDE.md` for baseline guidance.

Simple test framework with minimal API, designed for GitLab CI
integration.

## Key API

- `run` — execute tests
- `report` — generate results (text or HTML via cl-who)

## Dependencies

cl-who (for HTML report output)

## Conventions

- Package: `:cl-naive-tests`
- Tests: `cl-naive-tests.tests` (self-testing)
- Docs: `docs/docs.org`
- Default test framework across all NAIVE projects
