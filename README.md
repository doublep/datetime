# datetime

Datetime is a library for parsing, formatting, matching and recoding
timestamps and date-time format strings.  Not all of the planned
functionality is implemented yet.


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
* `datetime-pattern-includes-millisecond-p`
* `datetime-pattern-includes-timezone-p`

* `datetime-list-locales`
* `datetime-list-timezones`

* `datetime-locale-date-pattern`
* `datetime-locale-time-pattern`
* `datetime-locale-date-time-pattern`
* `datetime-locale-field`
