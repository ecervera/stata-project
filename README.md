# stata-project

## Prerequisites

1. [Docker](https://docs.docker.com/engine/install/)
2. A [Stata](https://www.stata.com/) license

## Instructions

1. Clone this repository
2. Copy your `stata.lic` file in the repository folder
3. Launch an interactive Stata container with `run_stata.sh`
4. Execute in the Stata prompt:
```
do Setup.do
do "Replication code.do"
```
