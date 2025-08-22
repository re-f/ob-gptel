# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

ob-gptel is an Org Babel backend for gptel that enables AI-powered interactions directly within Org mode source blocks. It allows seamless integration of AI responses into Org documents.

## Key Architecture

The codebase consists of a single main file `ob-gptel.el` that implements the Org Babel backend:

- **Core execution**: `org-babel-execute:gptel` handles executing gptel source blocks
- **Session support**: Functions like `ob-gptel-find-session` and `ob-gptel--all-source-blocks` enable conversation context
- **Completion support**: `ob-gptel-capf` provides completion-at-point for header arguments
- **Integration with gptel**: Uses gptel's request system with presets, backends, and models

## Development Commands

Since this is an Emacs Lisp package, there are no build/test commands. Development is typically done within Emacs:

- **Load the package**: `(require 'ob-gptel)` after adding to load-path
- **Enable for Org Babel**: Add `(gptel . t)` to `org-babel-load-languages`
- **Test functionality**: Create a `.org` file with gptel source blocks and execute with `C-c C-c`

## Header Arguments

The package supports these header arguments for gptel blocks:
- `:model` - GPT model (e.g., gpt-4)
- `:temperature` - Sampling temperature (0.0-2.0)
- `:max-tokens` - Maximum response tokens
- `:system` - System message for context
- `:backend` - gptel backend to use
- `:session` - Session name for conversation context
- `:prompt` - Reference to previous block as context
- `:preset` - Name of gptel preset to use
- `:dry-run` - Show request without sending
- `:context` - Additional files to include as context