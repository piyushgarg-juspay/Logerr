
# Haskell Code Checker Plugin

## Overview

This Haskell plugin automatically checks function calls for given rule violations. It analyzes the source code to identify function calls and checks them against defined rule if that function application violates the rule. 
It supports the following rules as of now
1. Blocking certain type of argument to a function call
2. Blocking use of certain function in an argument to a function call
3. Blocking use of a particular function in the code (specify argument as `0` in the rules)

This tool is useful for developers to enforce correct coding practices and prevent use of some specific unsafe function in the code.

## Usage

Add this to your ghc-options in cabal and mention logerr it in build-depends

```
-fplugin=Logerr.Plugin
```