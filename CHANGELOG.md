# Change Log
All notable changes to this project will be documented in this file. This
project adheres to [Semantic Versioning](http://semver.org/) and Haskell
[package versioning policy](https://wiki.haskell.org/Package_versioning_policy).


## [Unreleased][unreleased]

### Changed
- Exporting `tablifyCsvLinks` function (Merged pull request by @vmandela)

### Added
- Added `stack.yaml`.


## [1.0.3] - 2016-02-07

### Changed
- Updated cabal file

### Fixed
- Handling `tablifyCsvLinks` conditionally using CPP macro (fixes issue #17).


## [1.0.1] - 2015-06-02

### Fixed
- Now handling `readMarkdown` conditionally using CPP macro (fixes issue #8).


## 1.0.0
- The first release.

[unreleased]: https://github.com/baig/pandoc-csv2table-filter/compare/1.0.3...HEAD
[1.0.2]: https://github.com/baig/pandoc-csv2table-filter/compare/1.0.1...1.0.3
[1.0.1]: https://github.com/baig/pandoc-csv2table-filter/compare/1.0.0...1.0.1