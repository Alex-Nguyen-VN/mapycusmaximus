# Contributing to mapycusmaximus

Thank you for your interest in contributing to `mapycusmaximus`! We welcome contributions from the community to help improve this package for focus-context visualization and fisheye transformations in R.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [How to Contribute](#how-to-contribute)
- [Development Workflow](#development-workflow)
- [Coding Standards](#coding-standards)
- [Testing](#testing)
- [Documentation](#documentation)
- [Commit Guidelines](#commit-guidelines)
- [Pull Request Process](#pull-request-process)
- [Getting Help](#getting-help)

---

## Code of Conduct

By participating in this project, you agree to maintain a respectful and inclusive environment. We expect all contributors to:

- Be respectful and considerate in communication
- Welcome diverse perspectives and experiences
- Accept constructive criticism gracefully
- Focus on what is best for the community
- Show empathy towards other community members

---

## Getting Started

### Prerequisites

Before contributing, ensure you have:

- **R** (version ≥ 4.0.0 recommended)
- **RStudio** (optional but recommended)
- **Git** for version control
- **devtools** package: `install.packages("devtools")`
- **roxygen2** package: `install.packages("roxygen2")`
- **testthat** package: `install.packages("testthat")`

### Fork and Clone the Repository

1. **Fork** the repository on GitHub by clicking the "Fork" button
2. **Clone** your fork locally:

```bash
git clone https://github.com/YOUR-USERNAME/mapycusmaximus.git
cd mapycusmaximus
```

3. **Add the upstream repository** as a remote:

```bash
git remote add upstream https://github.com/Alex-Nguyen-VN/mapycusmaximus.git
```

4. **Install the package dependencies**:

```r
devtools::install_deps()
```

5. **Load the package for development**:

```r
devtools::load_all()
```

---

## How to Contribute

### Types of Contributions

We welcome various types of contributions:

- **Bug reports**: Report issues you encounter
- **Feature requests**: Suggest new functionality
- **Bug fixes**: Submit fixes for known issues
- **New features**: Add new transformation methods or utilities
- **Documentation improvements**: Enhance README, vignettes, or function documentation
- **Performance improvements**: Optimize existing code
- **Test coverage**: Add or improve unit tests

### Reporting Bugs

If you find a bug, please create an issue with:

- A clear, descriptive title
- Steps to reproduce the bug
- Expected vs. actual behavior
- Your R version and package version
- Minimal reproducible example (reprex)
- Relevant error messages or warnings

**Example template:**

```markdown
**Bug Description:**
Brief description of the issue

**To Reproduce:**
1. Load the package
2. Run the following code:
   ```r
   # Your minimal reproducible example
   ```
3. Observe the error

**Expected Behavior:**
What should happen

**Environment:**
- R version: 4.3.0
- mapycusmaximus version: 1.0.7
- OS: macOS 14.0

**Additional Context:**
Any other relevant information
```

### Suggesting Enhancements

For feature requests:

- Check if the feature already exists or has been requested
- Provide a clear use case for the feature
- Explain how it aligns with the package's scope (FGC fisheye transformations)
- Include example code showing how the feature would be used

---

## Development Workflow

### 1. Create a Feature Branch

Always work on a separate branch:

```bash
git checkout -b feature/your-feature-name
```

Or for bug fixes:

```bash
git checkout -b fix/bug-description
```

### 2. Make Your Changes

- Write clean, readable code following our [Coding Standards](#coding-standards)
- Add or update tests as needed
- Update documentation for any changed functionality
- Ensure your code passes R CMD check

### 3. Test Your Changes

Run the following checks locally:

```r
# Load all package functions
devtools::load_all()

# Run tests
devtools::test()

# Check package
devtools::check()

# Build documentation
devtools::document()
```

### 4. Commit Your Changes

Follow our [Commit Guidelines](#commit-guidelines) when committing:

```bash
git add .
git commit -m "feat: Add new fisheye method for radial distortion"
```

### 5. Push and Create Pull Request

```bash
git push origin feature/your-feature-name
```

Then create a pull request on GitHub.

---

## Coding Standards

### R Code Style

We follow the **tidyverse style guide** with some modifications:

#### Naming Conventions

- **Functions**: Use snake_case
  ```r
  fisheye_fgc()
  sf_fisheye()
  plot_fisheye_fgc()
  ```

- **Variables**: Use snake_case
  ```r
  zoom_factor <- 1.5
  r_in <- 0.34
  ```

- **Constants**: Use UPPER_SNAKE_CASE
  ```r
  DEFAULT_ZOOM <- 1.5
  MAX_ITERATIONS <- 100
  ```

#### Formatting

- **Indentation**: 2 spaces (no tabs)
- **Line length**: Maximum 80 characters
- **Spacing**: 
  ```r
  # Good
  x <- 1 + 2
  my_function(arg1 = value1, arg2 = value2)
  
  # Bad
  x<-1+2
  my_function(arg1=value1,arg2=value2)
  ```

- **Braces**:
  ```r
  # Good
  if (condition) {
    do_something()
  } else {
    do_something_else()
  }
  
  # Bad
  if (condition)
  {
    do_something()
  }
  ```

#### Function Documentation

All exported functions must have roxygen2 documentation:

```r
#' Apply FGC Fisheye Transformation
#'
#' Transforms coordinates using the Focus-Glue-Context model
#'
#' @param coords A matrix or data frame of coordinates (x, y)
#' @param r_in Numeric. Radius of the focus region (default: 0.34)
#' @param r_out Numeric. Outer radius of the glue region (default: 0.50)
#' @param zoom_factor Numeric. Magnification factor for focus region (default: 1.5)
#' @param squeeze_factor Numeric. Compression in glue region (0, 1] (default: 0.3)
#'
#' @return A matrix of transformed coordinates
#'
#' @examples
#' grid <- create_test_grid(range = c(-1, 1), spacing = 0.1)
#' transformed <- fisheye_fgc(grid, r_in = 0.34, r_out = 0.5)
#'
#' @export
fisheye_fgc <- function(coords, r_in = 0.34, r_out = 0.50, 
                        zoom_factor = 1.5, squeeze_factor = 0.3) {
  # Function body
}
```

#### Dependencies

- Use `::` notation for functions from other packages (except base R)
- Declare all dependencies in DESCRIPTION file
- Import specific functions in NAMESPACE using roxygen2:
  ```r
  #' @importFrom sf st_transform st_crs
  ```

---

## Testing

### Writing Tests

We use `testthat` for unit testing. Tests should be placed in `tests/testthat/`.

#### Test Structure

```r
# tests/testthat/test-fisheye_fgc.R

test_that("fisheye_fgc preserves input dimensions", {
  coords <- matrix(c(1, 2, 3, 4), ncol = 2)
  result <- fisheye_fgc(coords)
  
  expect_equal(nrow(result), nrow(coords))
  expect_equal(ncol(result), ncol(coords))
})

test_that("fisheye_fgc handles edge cases", {
  # Test with origin
  coords <- matrix(c(0, 0), ncol = 2)
  result <- fisheye_fgc(coords)
  expect_equal(result, coords)
  
  # Test with negative coordinates
  coords <- matrix(c(-1, -1), ncol = 2)
  expect_silent(fisheye_fgc(coords))
})

test_that("fisheye_fgc validates parameters", {
  coords <- matrix(c(1, 2), ncol = 2)
  
  expect_error(fisheye_fgc(coords, r_in = -1))
  expect_error(fisheye_fgc(coords, zoom_factor = 0.5))
  expect_error(fisheye_fgc(coords, squeeze_factor = 1.5))
})
```

#### Coverage Goals

- Aim for >80% test coverage
- Test edge cases and error conditions
- Include tests for all exported functions
- Test integration with sf objects

#### Running Tests

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-fisheye_fgc.R")

# Check test coverage
covr::package_coverage()
```

---

## Documentation

### Function Documentation

- Use roxygen2 for all exported functions
- Include clear parameter descriptions
- Provide at least one working example
- Document return values and types
- Note any side effects or warnings

### Vignettes

When adding new features, consider creating a vignette:

```r
usethis::use_vignette("new-feature-name")
```

Vignettes should:
- Explain the feature's purpose and use cases
- Provide working examples
- Include visualizations where appropriate
- Follow a logical flow from simple to complex usage

### README Updates

Update README.md if your changes:
- Add new major features
- Change the installation process
- Modify core functionality
- Add new dependencies

---

## Commit Guidelines

We follow the **Conventional Commits** specification.

### Commit Message Format

```
<type>(<scope>): <subject>

<body>

<footer>
```

### Types

- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `style`: Code style changes (formatting, no logic change)
- `refactor`: Code refactoring
- `perf`: Performance improvements
- `test`: Adding or updating tests
- `chore`: Maintenance tasks (dependencies, build, etc.)
- `ci`: CI/CD changes

### Examples

```bash
# Feature
git commit -m "feat: add revolution parameter for angular twist"

# Bug fix
git commit -m "fix: correct polygon closure in sf_fisheye"

# Documentation
git commit -m "docs: update README with new center specification examples"

# Multiple changes
git commit -m "feat: implement outward expansion method

- Add method parameter to fisheye_fgc
- Update documentation with method comparison
- Add tests for both expansion methods"

# Breaking change
git commit -m "feat!: change default zoom_factor to 2.0

BREAKING CHANGE: Default zoom_factor increased from 1.5 to 2.0"
```

### Commit Best Practices

- Make atomic commits (one logical change per commit)
- Write clear, descriptive messages
- Reference related issues (e.g., "fixes #42")
- Keep commit history clean (rebase if needed)

---

## Pull Request Process

### Before Submitting

Ensure your pull request:

1. **Passes all checks**:
   ```r
   devtools::check()
   ```

2. **Has appropriate test coverage**:
   ```r
   covr::package_coverage()
   ```

3. **Updates documentation**:
   ```r
   devtools::document()
   ```

4. **Follows code style guidelines**

5. **Includes a clear description**

### Pull Request Template

```markdown
## Description
Brief description of changes

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Breaking change
- [ ] Documentation update

## Changes Made
- Change 1
- Change 2

## Testing
Describe testing performed

## Checklist
- [ ] Code follows style guidelines
- [ ] Tests added/updated
- [ ] Documentation updated
- [ ] R CMD check passes
- [ ] NEWS.md updated (if applicable)

## Related Issues
Closes #123
```

### Review Process

1. **Automated checks**: CI/CD runs R CMD check
2. **Code review**: Maintainers review your code
3. **Feedback**: Address any requested changes
4. **Approval**: Minimum one approval required
5. **Merge**: Maintainer merges your PR

### After Merging

- Delete your feature branch
- Update your local main branch:
  ```bash
  git checkout main
  git pull upstream main
  ```

---

## Package Structure

Understanding the package structure helps when contributing:

```
mapycusmaximus/
├── R/                      # R source code
│   ├── fisheye_fgc.R      # Core transformation functions
│   ├── sf_fisheye.R       # SF integration
│   └── utils.R            # Utility functions
├── man/                    # Auto-generated documentation
├── tests/
│   └── testthat/          # Unit tests
├── vignettes/             # Package vignettes
├── data/                  # Example datasets
├── data-raw/              # Code to create data/
├── inst/                  # Installed files
├── DESCRIPTION            # Package metadata
├── NAMESPACE              # Auto-generated exports
├── README.md              # Main documentation
├── NEWS.md               # Changelog
└── LICENSE.md            # MIT License
```

---

## Getting Help

If you need assistance:

1. **Check existing issues** on GitHub
2. **Review documentation** at https://alex-nguyen-vn.github.io/mapycusmaximus/
3. **Create a discussion** for questions
4. **Email the maintainer** for private inquiries

---

## Recognition

Contributors will be:
- Listed in the package DESCRIPTION file
- Acknowledged in NEWS.md for significant contributions
- Credited in release notes

---

## License

By contributing to mapycusmaximus, you agree that your contributions will be licensed under the MIT License.

---

## Additional Resources

- [R Packages Book](https://r-pkgs.org/)
- [Tidyverse Style Guide](https://style.tidyverse.org/)
- [Writing R Extensions](https://cran.r-project.org/doc/manuals/R-exts.html)
- [testthat Documentation](https://testthat.r-lib.org/)
- [roxygen2 Documentation](https://roxygen2.r-lib.org/)

---

**Thank you for contributing to mapycusmaximus! Your efforts help make spatial data visualization more accessible and powerful for the R community.**
