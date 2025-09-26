# Using renv for Package Management: Developer's Guide

## Overview

We use [`renv`](https://rstudio.github.io/renv/) to facilitate reproducibility and manage project-specific R packages.
It uses a lockfile (`renv.lock`) to record the exact package versions used in the project.

To use renv, uncomment the `source("renv/activate.R")` line.  

---

## For New Users & Collaborators

### 1. Clone the repository and restore the environment

```sh
git clone git@github.com:ccmmf/downscaling
cd downscaling
R -e "renv::restore()"
```
- This will install all required packages as specified in `renv.lock`.

### 2. Activate the renv environment

```r
renv::activate()
```
- This is usually done automatically when you open the project in RStudio or VS Code.

> If you get `Error in loadNamespace(x) : there is no package called 'renv'`,  
> ensure `R_LIBS_USER` is correctly set to the platform and R version specific path (see `.Renviron`),  
> or install `renv` globally using:
>
> ```r
> install.packages("renv")
> ```

---

## For Maintainers & Developers

### 1. Adding or updating packages

- Install new packages as usual:
  ```r
  install.packages("somepackage")
  # or for GitHub packages:
  renv::install("username/repo@branch")
  ```
- renv will track these changes.

### 2. Recording dependencies

- By default, `renv::snapshot()` will **automatically detect dependencies** by scanning your project files (R scripts, Rmd, qmd, etc.).
- To create or update the lockfile:
  ```r
  renv::snapshot()
  ```
- This will write the current state of your library to `renv.lock`.

### 3. Checking project status

- To see if your library and lockfile are in sync:
  ```r
  renv::status()
  ```

### 4. Inspecting dependencies

- To see which packages are used and where:
  ```r
  renv::dependencies()
  ```

---

## Installing a Specific Package Version from GitHub

```r
renv::install("username/repo@branch")
# For a subdirectory (e.g., PEcAnAssimSequential):
renv::install("dlebauer/pecan@ensemble_downscaling", subdir = "modules/assim.sequential")
```

---

## Best Practices

- **Always run `renv::snapshot()` after adding, removing, or updating packages.**
- **Commit the updated `renv.lock` to version control.**
- **Collaborators should run `renv::restore()` after pulling changes to `renv.lock`.**
- If you want to control which files are scanned for dependencies, see `?renv::dependencies` and consider using `renv::settings$snapshot.type("explicit")`.

---

## Troubleshooting

- If a package is missing or not available on CRAN, install it from GitHub or another source as needed.
- If you encounter errors, check `.libPaths()` and your `.Renviron` for correct library paths.
- Use package from a different library path if necessary:
```r
pkg <- "curl"
renv::hydrate(pkg, library = "/usr2/collab/dlebaue1/ccmmf/usr/dlebauer/R/x86_64-pc-linux-gnu-library/4.4")
```
- Copy package from a different library path:

```r

pkg <- "curl"
file.copy(
  from = file.path('/usr2/collab/dlebaue1/ccmmf/usr/dlebauer/R/x86_64-pc-linux-gnu-library/4.4', pkg),
  to = file.path(Sys.getenv("RENV_PATHS_LIBRARY")),
  recursive = TRUE
)

---

## References

- [renv documentation](https://rstudio.github.io/renv/articles/renv.html)


### How this was set up 

```r
options(repos = c(
    pecanproject = "https://pecanproject.r-universe.dev",
    ajlyons      = "https://ajlyons.r-universe.dev", # caladaptr
    CRAN         = "https://cloud.r-project.org"
))
.libPaths("/usr2/collab/dlebaue1/ccmmf/usr/dlebauer/R/x86_64-pc-linux-gnu-library/4.4")
renv::init(bioconductor = TRUE)
```