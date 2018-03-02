# Change Log
All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased][unreleased]

## [2.4.0] - 02-03-2018
### Added
    - [Merge .appup.pre.src & .appup.post.src files into the generated appup](https://github.com/lrascao/rebar3_appup_utils/pull/31)
    - [Add documentation for dependency custom appup files](https://github.com/lrascao/rebar3_appup_utils/pull/33)
    - [Add support of custom branches on relapp tests](https://github.com/lrascao/rebar3_appup_utils/pull/32)
### Changed
### Fixed
    - [Replace deprecated string:tokens/2](https://github.com/lrascao/rebar3_appup_utils/pull/42)
    - [Replace deprecated string:tokens/2 and string:strip/3](https://github.com/lrascao/rebar3_appup_utils/pull/40)
    - [Drop unknown behaviours when generating appup](https://github.com/lrascao/rebar3_appup_utils/pull/34)
    - [Fetch dep app info from all_deps instead of parsed deps](https://github.com/lrascao/rebar3_appup_utils/pull/26)

## [2.3.0] - 05-12-2017
### Added
    * [Add support for appup.src dependency overrides](https://github.com/lrascao/rebar3_appup_utils/pull/19)
### Changed
    * [Bump bbmustache to 1.5.0](https://github.com/lrascao/rebar3_appup_utils/pull/18)
### Fixed
    * [Fix/capital name modules](https://github.com/lrascao/rebar3_appup_utils/pull/21)
    * [Properly check appup existence before generating](https://github.com/lrascao/rebar3_appup_utils/pull/25)

## [2.2.1] - 16-08-2017
### Added
    * Add debug on clean operation
    * Add Travis support for OTP20
    * Disable export_all warning while running tests
### Changed
### Fixed
    * Support multiple behaviour module upgrades

## [2.2.0] - 18-02-2017
### Added
    * Add rebar3 state variable available to appup.src scripts
    * Support automatic stop/start of supervised workers
    * Support add/remove of application dependencies
### Changed
### Fixed
