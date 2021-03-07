[![License: GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA Stable](http://stable.melpa.org/packages/datetime-badge.svg)](http://stable.melpa.org/#/datetime)
[![MELPA](http://melpa.org/packages/datetime-badge.svg)](http://melpa.org/#/datetime)
[![CI](https://github.com/doublep/datetime/workflows/CI/badge.svg)](https://github.com/doublep/datetime/actions?query=workflow%3ACI)


# datetime

Datetime is a library for parsing, formatting, matching and recoding
timestamps and date-time format strings.  Not all of the planned
functionality is implemented yet.


## System locale and timezone

The library will try to guess your system locale and timezone, but
this is frustratingly difficult.  In particular, on MS Windows it will
not be able to determine the timezone (not sure about locale).
Patches to improve this are welcome.

In any case, when it fails completely or guesses incorrectly, you can
always override the heuristics results by setting variables
`datetime-locale` and/or `datetime-timezone` manually.  Both are also
available through Emacs customization interface, group `datetime`.

To find the list of all supported locales and timezones, evaluate the
following forms:

    (prin1-to-string (sort (datetime-list-locales t) #'string<))
    (prin1-to-string (sort (datetime-list-timezones) #'string<))


## Pattern types

There exist several different ways to specify date and/or time format.
Different programming language and their libraries use different
formats.  E.g. format of date 2015-09-24 can be expressed at least in
the following ways:

* `yyyy-MM-dd` ([ICU](http://userguide.icu-project.org/formatparse/datetime), [Java](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html))
* `%Y-%m-%d` (POSIX, Emacs)
* `Y-m-d` ([PHP](http://php.net/manual/en/function.date.php))

This library currently uses Java pattern format, but is internally
written in such a way that support for other types can be added
relatively easily.


## Parsing timestamps

Parsing timestamps using the library is a two-step operation.  First
you create a *parser function* for a specific pattern and options.
Then call the parser function with one argument — the timestamp as a
string.  It returns a floating point number — number of seconds since
UNIX epoch in UTC timezone.

Create a parser:

    (datetime-parser-to-float 'java "yyyy-MM-dd HH:mm:ss.SSS"
                              :timezone 'system)

And use it:

    (let ((parser (datetime-parser-to-float 'java "yyyy-MM-dd HH:mm:ss.SSS"
                                            :timezone 'system)))
      (funcall parser "2018-09-18 21:20:00.000"))

Remember that if you parse timestamps formatted on your machine, you
need to pass `'system` as `:timezone` option to
`datetime-parser-to-float`: default timezone is UTC.  Parsing
timestamps with varying timezones (i.e. with timezone information
directly in the input string) is not yet supported.


## Formatting timestamps

To format timestamps you first need to create a *formatter function*.
This function accepts one argument — the timestamp as a floating point
number — and converts it to a string.  All customization, most
importantly, specifying the pattern, is done at the point of creating
the formatter.

For example:

    (datetime-float-formatter 'java "yyyy-MM-dd HH:mm:ss.SSS"
                              :timezone 'system)

With this formatter function you can now format timestamps as follows:

    (let ((formatter (datetime-float-formatter 'java "yyyy-MM-dd HH:mm:ss.SSS"
                                               :timezone 'system)))
      (funcall formatter (float-time)))

Note that if you plan to format result of `float-time` function, you
need to pass `'system` as `:timezone` option to
`datetime-float-formatter`: default timezone is UTC.

Starting with version 0.7 the library partially supports formatting
timezone names: `z` and `zzzz` in Java patterns can be used to format
abbreviated of full names.  For example:

    (let ((formatter (datetime-float-formatter 'java "HH:mm:ss z"
                                               :timezone 'system)))
      (funcall formatter (float-time)))


## Matching timestamps

Sometimes you need to determine if given string is (likely) a
timestamp, corresponding to given pattern.  A robust way, of course,
is to try to parse it.  However, it is much faster, though not as
precise, to use a regular expression.

Function `datetime-matching-regexp` builds such a regular expression
for given pattern.  For example,

    (datetime-matching-regexp 'java "yyyy-MM-dd HH:mm:ss.SSS")

returns a regexp that will match all timestamp strings produced by the
formatter we created earlier.  It will also match some other strings,
but is good enough in practice to tell if “this does look like a
timestamp”.


## Other functions

These functions are also part of the library interface.  They are
documented within Emacs.

* `datetime-recode-pattern`

* `datetime-pattern-locale-dependent-p`
* `datetime-pattern-includes-date-p`
* `datetime-pattern-includes-time-p`
* `datetime-pattern-includes-era-p`
* `datetime-pattern-includes-year-p`
* `datetime-pattern-includes-month-p`
* `datetime-pattern-includes-week-p`
* `datetime-pattern-includes-day-p`
* `datetime-pattern-includes-weekday-p`
* `datetime-pattern-includes-hour-p`
* `datetime-pattern-includes-minute-p`
* `datetime-pattern-includes-second-p`
* `datetime-pattern-includes-second-fractionals-p`
* `datetime-pattern-num-second-fractionals`
* `datetime-pattern-includes-timezone-p`

* `datetime-list-locales`
* `datetime-list-timezones`

* `datetime-locale-date-pattern`
* `datetime-locale-time-pattern`
* `datetime-locale-date-time-pattern`
* `datetime-locale-field`
* `datetime-locale-timezone-name`

* `datetime-locale-database-version`
* `datetime-timezone-database-version`
* `datetime-timezone-name-database-version`
