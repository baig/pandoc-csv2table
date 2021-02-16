# Change Log
All notable changes to this project will be documented in this file. This
project adheres to [Semantic Versioning](http://semver.org/) and Haskell
[package versioning policy](https://wiki.haskell.org/Package_versioning_policy).

## [Unreleased]

- Update for change to table definition in pandoc-types 1.21.0
- Added test procedure in lieu of a full test suite

## [1.0.8] - 2020-01-27

- update stack version
- Fix build failure with pandoc 1.17
- Fix build failure with pandoc 2.9

## [1.0.7] - 2019-06-02

- Export `tablifyCSVLinksPure` for cases where no IO is required.
- Handle change in Pandoc markdown reader default options in pandoc-csv2table
  See <https://github.com/jgm/pandoc/commit/a58369a7e65075800>
- Update stack version

## [1.0.6] - 2018-11-29

- Made compatibile with Pandoc 2.0+ (Merged pull request by @vmandela)
- Updated cabal file
- Updated stack version

## [1.0.5] - 2017-05-05


## [1.0.4] - 2016-02-07

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


[Unreleased]: https://github.com/baig/pandoc-csv2table-filter/compare/1.0.8...HEAD
[1.0.8]: https://github.com/baig/pandoc-csv2table-filter/compare/1.0.7...1.0.8
[1.0.7]: https://github.com/baig/pandoc-csv2table-filter/compare/1.0.6...1.0.7
[1.0.6]: https://github.com/baig/pandoc-csv2table-filter/compare/1.0.5...1.0.6
[1.0.5]: https://github.com/baig/pandoc-csv2table-filter/compare/1.0.4...1.0.5
[1.0.4]: https://github.com/baig/pandoc-csv2table-filter/compare/1.0.3...1.0.4
[1.0.2]: https://github.com/baig/pandoc-csv2table-filter/compare/1.0.1...1.0.3
[1.0.1]: https://github.com/baig/pandoc-csv2table-filter/compare/1.0.0...1.0.1
