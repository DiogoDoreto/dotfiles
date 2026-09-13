# Dotfiles

Personal system and application configuration shared across the user's machines.

## Language

**Nix package record**:
A complete package search result returned by `nh`, containing the package attribute and all information needed to present package details.
_Avoid_: Candidate, package details

**Package details**:
A human-readable view of a Nix package record.
_Avoid_: Package lookup

**Agent VM**:
An isolated machine in which coding agents and their control surfaces operate with their own persistent state.
_Avoid_: OpenCode VM

**Agent control surface**:
A protected web interface for observing or directing coding agents running in the Agent VM.
_Avoid_: Agent webserver, desktop app

**Paseo daemon**:
The headless Agent VM process that manages coding agents and serves Paseo's Agent control surface.
_Avoid_: Paseo Desktop, Paseo app
