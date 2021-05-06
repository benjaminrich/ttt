# Version 1.0

This is the first submission of `ttt` to CRAN.

## Fixing the following comments from reviewer and resubmitting

* From: Gregor Seyer <gregor.seyer@wu.ac.at>
* Date: Thu, 6 May 2021 09:56:56 +0200

  - Please do not start the description with "This package", package name,
    title or similar.

    * Changed description.

  - Please write TRUE and FALSE instead of T and F. (Please don't use 'T' or
    'F' as vector names.)

    * Done.

  - Please add \value to .Rd files regarding exported methods and explain the
    functions results in the documentation. Please write about the structure of
    the output (class) and also what the output means. (If a function does not
    return a value, please document that too, e.g. \value{No return value,
    called for side effects} or similar)

    Missing Rd-tags:
    knit_print.ttt.Rd: \value

    * \value added.

## Test environments

* Local:
  - NixOS (Linux), R 4.0.4 (x86_64-pc-linux-gnu)
  - Windows 10, R 4.0.5 (x86_64-w64-mingw32/x64 (64-bit))
* travis-ci:
  - Ubuntu Linux 16.04.6 LTS (xenial) (release and devel)
* win-builder:
  - Windows Server 2008 (release (4.0.5) and devel (4.1.0 alpha))
    - 1 NOTE:
      * New submission
* R-hub builder (https://builder.r-hub.io)
  - Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    - 1 NOTE:
      * New submission
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
    - 1 NOTE:
      * New submission
  - Fedora Linux, R-devel, clang, gfortran
    - 2 NOTEs:
      * New submission
      * unable to verify current time

## R CMD check results

0 errors | 0 warnings | 1 note

* unable to verify current time

## Reverse dependencies

There are currently no downstream dependencies for this package.
