import java.time.*;
import java.time.chrono.*;
import java.time.format.*;
import java.time.temporal.*;
import java.time.zone.*;
import java.util.*;
import java.util.function.*;
import java.util.stream.*;


public class HarvestData
{
    private static final long    DAYS_IN_400_YEARS       = IntStream.range (0, 400).map ((year) -> isLeapYear (year) ? 366 : 365).sum ();
    private static final long    SECONDS_IN_400_YEARS    = (DAYS_IN_400_YEARS * 24 * 60 * 60);
    private static final long    AVERAGE_SECONDS_IN_YEAR = (SECONDS_IN_400_YEARS / 400);
    private static final String  ENGLISH_ERAS            = toLispVector (getNames (Locale.ENGLISH, ChronoField.ERA,         "G", 0, 1));
    private static final String  ENGLISH_AM_PM           = toLispVector (getNames (Locale.ENGLISH, ChronoField.AMPM_OF_DAY, "a", 0, 1));


    public static void main (String[] args) throws Exception
    {
        if (Arrays.asList (args).contains ("--locales"))
            printLocaleData ();

        if (Arrays.asList (args).contains ("--timezones"))
            printTimezoneData ();

        if (Arrays.asList (args).contains ("--timezone-names"))
            printTimezoneNameData ();
    }


    protected static List <Locale> getAllLocales ()
    {
        List <Locale>  locales = new ArrayList <> (Arrays.asList (Locale.getAvailableLocales ()));

        locales.removeIf ((locale) -> {
                // This way we discard a few locales that can otherwise lead to duplicate keys
                // because of use of toLanguageTag().  E.g. `no_NO_NY' is problematic.
                if (locale.getVariant ().length () > 0)
                    return true;

                if (!Chronology.ofLocale (locale).getId ().equals ("ISO")) {
                    // Ignore such locales for now.
                    return true;
                }

                return false;
            });

        locales.sort ((a, b) -> a.toLanguageTag ().compareToIgnoreCase (b.toLanguageTag ()));
        return locales;
    }

    protected static List <ZoneId> getAllTimezones ()
    {
        List <ZoneId>  timezones = ZoneId.getAvailableZoneIds ().stream ().map ((id) -> ZoneId.of (id)).collect (Collectors.toList ());
        timezones.sort ((a, b) -> a.getId ().compareToIgnoreCase (b.getId ()));
        return timezones;
    }


    protected static void printLocaleData () throws Exception
    {
        Map <Locale, Map <String, String>>  data = new LinkedHashMap <> ();

        for (Locale locale : getAllLocales ()) {
            Chronology  chronology = Chronology.ofLocale (locale);

            Map <String, String>  map = new LinkedHashMap <> ();
            data.put (locale, map);

            map.put (":decimal-separator",        String.format ("?%c", DecimalStyle.of (locale).getDecimalSeparator ()));
            map.put (":eras",                     toLispVector (getNames (locale, ChronoField.ERA,           "G",    0, 1)));
            map.put (":month-context-abbr",       toLispVector (getNames (locale, ChronoField.MONTH_OF_YEAR, "MMM",  1, 12)));
            map.put (":month-context-names",      toLispVector (getNames (locale, ChronoField.MONTH_OF_YEAR, "MMMM", 1, 12)));
            map.put (":weekday-context-abbr",     toLispVector (getNames (locale, ChronoField.DAY_OF_WEEK,   "EEE",  1, 7)));
            map.put (":weekday-context-names",    toLispVector (getNames (locale, ChronoField.DAY_OF_WEEK,   "EEEE", 1, 7)));
            map.put (":month-standalone-abbr",    toLispVector (getNames (locale, ChronoField.MONTH_OF_YEAR, "LLL",  1, 12)));
            map.put (":month-standalone-names",   toLispVector (getNames (locale, ChronoField.MONTH_OF_YEAR, "LLLL", 1, 12)));
            map.put (":weekday-standalone-abbr",  toLispVector (getNames (locale, ChronoField.DAY_OF_WEEK,   "ccc",  1, 7)));
            map.put (":weekday-standalone-names", toLispVector (getNames (locale, ChronoField.DAY_OF_WEEK,   "cccc", 1, 7)));
            map.put (":am-pm",                    toLispVector (getNames (locale, ChronoField.AMPM_OF_DAY,   "a",    0, 1)));
            map.put (":day-periods",              findDayPeriodData (locale));

            Map <String, String>  date_patterns = toPatternPlist ((style) -> DateTimeFormatterBuilder.getLocalizedDateTimePattern (style, null, chronology, locale));
            Map <String, String>  time_patterns = toPatternPlist ((style) -> DateTimeFormatterBuilder.getLocalizedDateTimePattern (null, style, chronology, locale));

            // Fallbacks: short <- medium; full <- long <- medium.
            for (Map <String, String> patterns : Arrays.asList (date_patterns, time_patterns)) {
                if (Objects.equals (patterns.get (":short"), patterns.get (":medium")))
                    patterns.remove (":short");
                if (Objects.equals (patterns.get (":full"), patterns.get (":long")))
                    patterns.remove (":full");
                if (Objects.equals (patterns.get (":long"), patterns.get (":medium")))
                    patterns.remove (":long");
            }

            map.put (":date-patterns", toLispPlist (date_patterns, true));
            map.put (":time-patterns", toLispPlist (time_patterns, true));

            Map <List <String>, Boolean>  date_part_first            = new LinkedHashMap <> ();
            Map <List <String>, String>   constant_strings_per_style = new LinkedHashMap <> ();

            for (FormatStyle date_style : FormatStyle.values ()) {
                for (FormatStyle time_style : FormatStyle.values ()) {
                    List <String>  key = new ArrayList <> ();
                    for (FormatStyle style : new FormatStyle[] { date_style, time_style })
                        key.add (style == FormatStyle.SHORT ? ":short" : style == FormatStyle.MEDIUM ? ":medium" : style == FormatStyle.LONG ? ":long" : ":full");

                    String  date_pattern      = DateTimeFormatterBuilder.getLocalizedDateTimePattern (date_style, null,       chronology, locale);
                    String  time_pattern      = DateTimeFormatterBuilder.getLocalizedDateTimePattern (null,       time_style, chronology, locale);
                    String  date_time_pattern = DateTimeFormatterBuilder.getLocalizedDateTimePattern (date_style, time_style, chronology, locale);
                    int     at_1              = date_time_pattern.indexOf (date_pattern);
                    int     at_2              = date_time_pattern.indexOf (time_pattern);
                    int     length_1          = date_pattern.length ();
                    int     length_2          = time_pattern.length ();

                    if (at_1 < 0 || at_2 < 0 || (at_1 <= at_2 && at_1 + date_pattern.length () > at_2) || (at_2 <= at_1 && at_2 + time_pattern.length () > at_1)) {
                        throw new IllegalStateException (String.format ("cannot rebuild date-time pattern:\n  locale: %s\n  date-time: %s\n  date: %s\n  time: %s",
                                                                        locale.toLanguageTag (), date_time_pattern, date_pattern, time_pattern));
                    }

                    date_part_first.put (key, at_1 < at_2);

                    if (at_1 > at_2) {
                        int  temp = at_1;
                        at_1      = at_2;
                        at_2      = temp;
                        length_1  = time_pattern.length ();
                        length_2  = date_pattern.length ();
                    }

                    String  constant_strings = quoteString (date_time_pattern.substring (at_1 + length_1, at_2));

                    if (at_1 > 0 || at_2 + length_2 < date_time_pattern.length ()) {
                        constant_strings = toLispList (Arrays.asList (quoteString (date_time_pattern.substring (0, at_1)),
                                                                      constant_strings,
                                                                      quoteString (date_time_pattern.substring (at_2 + length_2))));
                    }

                    constant_strings_per_style.put (key, constant_strings);
                }
            }

            if (date_part_first.values ().stream ().distinct ().collect (Collectors.counting ()) == 1) {
                boolean  date_part_always_first = date_part_first.values ().stream ().distinct ().findFirst ().get ();
                if (constant_strings_per_style.values ().stream ().distinct ().collect (Collectors.counting ()) == 1)
                    map.put (":date-time-pattern-rule", String.format ("(%s . %s)", date_part_always_first ? "t" : "nil", constant_strings_per_style.values ().stream ().distinct ().findFirst ().get ()));
                else
                    map.put (":date-time-pattern-rule", String.format ("(%s . %s)", date_part_always_first ? "t" : "nil", toLispAlist (constant_strings_per_style, (key) -> toLispList (key), String::valueOf)));
            }
            else {
                for (List <String> key : date_part_first.keySet ())
                    constant_strings_per_style.put (key, String.format ("(%s . %s)", date_part_first.get (key) ? "t" : "nil", constant_strings_per_style.get (key)));

                map.put (":date-time-pattern-rule", toLispAlist (constant_strings_per_style, (key) -> toLispList (key), String::valueOf));
            }
        }

        for (Locale locale : getAllLocales ())
            removeUnnecessaryLocaleData (data, locale);

        System.out.println ("(");
        for (Map.Entry <Locale, Map <String, String>> entry : data.entrySet ())
            System.out.println (toLispPlist (entry.getKey ().toLanguageTag (), entry.getValue (), false));
        System.out.println (")");
    }

    protected static List <String> getNames (Locale locale, TemporalField field, String pattern, int from, int to)
    {
        // This day also was a Monday.
        LocalDateTime      local_date_time = LocalDateTime.of (-4, 1, 1, 6, 0, 0);
        DateTimeFormatter  formatter       = DateTimeFormatter.ofPattern (pattern, locale);
        List <String>      names           = new ArrayList <> ();

        for (int k = from; k <= to; k++) {
            if (local_date_time.getLong (field) != k)
                throw new IllegalStateException (String.format ("%s: expected %s = %s, but was %s", local_date_time, field, k, local_date_time.getLong (field)));

            names.add (formatter.format (local_date_time));

            if (field == ChronoField.AMPM_OF_DAY)
                local_date_time = local_date_time.plusHours (12);
            else if (field == ChronoField.ERA)
                local_date_time = local_date_time.plusYears (1000);
            else if (field == ChronoField.MONTH_OF_YEAR)
                local_date_time = local_date_time.plusMonths (1);
            else if (field == ChronoField.DAY_OF_WEEK)
                local_date_time = local_date_time.plusDays (1);
            else
                throw new IllegalArgumentException (field.toString ());
        }

        return names;
    }

    protected static String findDayPeriodData (Locale locale)
    {
        var            time       = LocalDateTime.of (2001, 1, 1, 0, 0, 0);
        var            formatters = List.of (DateTimeFormatter.ofPattern ("B",     locale),
                                             DateTimeFormatter.ofPattern ("BBBB",  locale),
                                             DateTimeFormatter.ofPattern ("BBBBB", locale));
        List <String>  strings    = null;
        var            periods    = new ArrayList <List <String>> ();
        var            thresholds = new ArrayList <Integer> ();

        // It seems the thresholds are not exposed, so we find them like this.
        for (int minute = 0; minute < 24 * 60; minute++, time = time.plusMinutes (1)) {
            var  time_       = time;
            var  new_strings = formatters.stream ().map ((formatter) -> formatter.format (time_)).toList ();

            if (!Objects.equals (new_strings, strings)) {
                periods.add (new_strings);
                if (minute > 0)
                    thresholds.add (minute);

                strings = new_strings;
            }
        }

        var  data = new LinkedHashMap <String, String> ();

        data.put (":thresholds", toLispList (thresholds));
        data.put (":short",      toLispVector (periods.stream ().map ((strings_) -> strings_.get (0)).toList (), true));
        data.put (":full",       toLispVector (periods.stream ().map ((strings_) -> strings_.get (1)).toList (), true));
        data.put (":narrow",     toLispVector (periods.stream ().map ((strings_) -> strings_.get (2)).toList (), true));

        if (Objects.equals (data.get (":full"), data.get (":short")))
            data.remove (":full");
        if (Objects.equals (data.get (":narrow"), data.get (":short")))
            data.remove (":narrow");

        return toLispPlist (data, false);
    }

    protected static void removeUnnecessaryLocaleData (Map <Locale, Map <String, String>> data, Locale locale)
    {
        Map <String, String>  locale_data = data.get (locale);
        Locale                parent      = new Locale (locale.getLanguage ());
        Map <String, String>  parent_data;

        if (Objects.equals (locale, parent))
            parent_data = new HashMap <> ();
        else {
            removeUnnecessaryLocaleData (data, parent);
            parent_data = data.get (parent);

            locale_data.put (":parent", parent.toLanguageTag ());
        }

        removeForFallback1 (locale_data, parent_data, ":decimal-separator",      "?.");
        removeForFallback1 (locale_data, parent_data, ":eras",                   ENGLISH_ERAS);
        removeForFallback1 (locale_data, parent_data, ":am-pm",                  ENGLISH_AM_PM);
        removeForFallback1 (locale_data, parent_data, ":day-periods",            null);
        removeForFallback1 (locale_data, parent_data, ":date-time-pattern-rule", "(t . \" \")");

        removeForFallback2 (locale_data, parent_data, ":month-standalone-abbr",    ":month-context-abbr");
        removeForFallback2 (locale_data, parent_data, ":month-standalone-names",   ":month-context-names");
        removeForFallback2 (locale_data, parent_data, ":weekday-standalone-abbr",  ":weekday-context-abbr");
        removeForFallback2 (locale_data, parent_data, ":weekday-standalone-names", ":weekday-context-names");

        for (Iterator <Map.Entry <String, String>> it = locale_data.entrySet ().iterator (); it.hasNext ();) {
            Map.Entry <String, String>  entry = it.next ();
            if (Objects.equals (entry.getValue (), parent_data.get (entry.getKey ())))
                it.remove ();
        }
    }

    protected static void removeForFallback1 (Map <String, String> locale_data, Map <String, String> parent_data, String key, String default_value)
    {
        if (Objects.equals (locale_data.get (key), parent_data.getOrDefault (key, default_value)))
            locale_data.remove (key);
    }

    protected static void removeForFallback2 (Map <String, String> locale_data, Map <String, String> parent_data, String main_key, String fallback_key)
    {
        if (Objects.equals (locale_data.get (main_key), locale_data.get (fallback_key)))
            locale_data.remove (main_key);
    }


    protected static void printTimezoneData () throws Exception
    {
        Map <ZoneId, List <Object>>  data = new LinkedHashMap <> ();

        for (ZoneId timezone : getAllTimezones ()) {
            ZoneRules  rules = timezone.getRules ();

            if (rules.isFixedOffset ())
                data.put (timezone, Collections.singletonList (rules.getOffset (Instant.now ()).getTotalSeconds ()));
            else {
                // They are probably already ordered, but I cannot find a confirmation in
                // the documentation.
                List <ZoneOffsetTransition>  transitions = new ArrayList <> (rules.getTransitions ());
                transitions.sort ((a, b) -> a.getInstant ().compareTo (b.getInstant ()));

                LocalDateTime         first           = LocalDateTime.ofInstant (transitions.get (0).getInstant (), ZoneOffset.UTC);
                int                   base_year       = Year.of (first.get (ChronoField.YEAR)).getValue ();
                long                  base            = Year.of (first.get (ChronoField.YEAR)).atDay (1).atStartOfDay ().toInstant (ZoneOffset.UTC).getEpochSecond ();
                int                   last_offset     = transitions.get (0).getOffsetBefore ().getTotalSeconds ();
                List <Object>         zone_data       = new ArrayList <> ();
                List <List <Object>>  transition_data = new ArrayList <> ();

                for (ZoneOffsetTransition transition : transitions) {
                    int  year_offset = (int) ((transition.getInstant ().getEpochSecond () - base) / AVERAGE_SECONDS_IN_YEAR);
                    if ((transition.getInstant ().getEpochSecond () + 1 - base) % AVERAGE_SECONDS_IN_YEAR < 1)
                        System.err.printf ("*Warning*: timezone '%s', offset transition at %s would be a potential rounding error\n", timezone.getId (), transition.getInstant ());

                    while (year_offset >= transition_data.size ())
                        transition_data.add (new ArrayList <> (Arrays.asList (last_offset)));

                    transition_data.get (year_offset).add (transition.getInstant ().getEpochSecond () - (base + year_offset * AVERAGE_SECONDS_IN_YEAR));
                    last_offset = transition.getOffsetAfter ().getTotalSeconds ();

                    // Floating-point offset is our internal mark of a transition to DST.
                    // Java is over-eager to convert ints to float for us, so we format
                    // them as strings manually now and add '.0' if appropriate.
                    boolean  to_dst = !Objects.equals (transition.getOffsetAfter (), rules.getStandardOffset (transition.getInstant ()));
                    transition_data.get (year_offset).add (String.format (to_dst ? "%d.0" : "%d", last_offset));
                }

                List <Object>  transition_rule_data = new ArrayList <> ();
                for (ZoneOffsetTransitionRule transition_rule : rules.getTransitionRules ()) {
                    Map <String, String>  rule = new LinkedHashMap <> ();

                    rule.put (":month",        String.valueOf (transition_rule.getMonth ().getValue ()));
                    rule.put (":day-of-month", String.valueOf (transition_rule.getDayOfMonthIndicator ()));

                    if (transition_rule.getDayOfWeek () != null)
                        rule.put (":day-of-week", String.valueOf (transition_rule.getDayOfWeek ().getValue () - 1));

                    if (transition_rule.isMidnightEndOfDay ())
                        rule.put (":end-of-day", "t");

                    rule.put (":time", String.valueOf (transition_rule.getLocalTime ().toSecondOfDay ()));

                    switch (transition_rule.getTimeDefinition ()) {
                    case UTC:
                        rule.put (":time-definition", "utc");
                        break;
                    case WALL:
                        rule.put (":time-definition", "wall");
                        break;
                    case STANDARD:
                        rule.put (":time-definition", "standard");
                        rule.put (":standard-offset", String.valueOf (transition_rule.getStandardOffset ().getTotalSeconds ()));
                        break;
                    default:
                        throw new IllegalStateException (transition_rule.getTimeDefinition ().name ());
                    }

                    rule.put (":before", String.valueOf (transition_rule.getOffsetBefore ().getTotalSeconds ()));
                    rule.put (":after",  String.valueOf (transition_rule.getOffsetAfter  ().getTotalSeconds ()));

                    if (!Objects.equals (transition_rule.getOffsetAfter (), transition_rule.getStandardOffset ()))
                        rule.put (":dst", "t");

                    transition_rule_data.add (toLispPlist (rule, false));
                }

                zone_data.add (String.valueOf (base));
                zone_data.add (toLispVector (transition_data.stream ().map (HarvestData::toLispList).collect (Collectors.toList ()), false));
                zone_data.add (String.valueOf (base_year));
                zone_data.add (toLispList (transition_rule_data));

                data.put (timezone, zone_data);
            }
        }

        System.out.println ("(");
        for (Map.Entry <ZoneId, List <Object>> entry : data.entrySet ())
            System.out.format ("(%s\n %s)\n", entry.getKey (), entry.getValue ().stream ().map (String::valueOf).collect (Collectors.joining ("\n ")));
        System.out.println (")");
    }


    protected static void printTimezoneNameData () throws Exception
    {
        Map <Locale, Map <String, String[]>>  data = new LinkedHashMap <> ();

        for (Locale locale : getAllLocales ()) {
            Map <String, String[]>  map = new LinkedHashMap <> ();

            DateTimeFormatter  abbreviation_retriever = DateTimeFormatter.ofPattern ("z",    locale);
            DateTimeFormatter  full_name_retriever    = DateTimeFormatter.ofPattern ("zzzz", locale);

            for (ZoneId timezone : getAllTimezones ()) {
                ZonedDateTime  dummy_date = ZonedDateTime.ofInstant (Instant.ofEpochSecond (0), timezone);
                String[]       names      = new String[] { abbreviation_retriever.format (dummy_date), null,
                                                           full_name_retriever   .format (dummy_date), null };
                ZoneRules      rules      = timezone.getRules ();


                if (!rules.isFixedOffset ()) {
                    // They are probably already ordered, but I cannot find a confirmation in
                    // the documentation.
                    List <ZoneOffsetTransition>  transitions = new ArrayList <> (rules.getTransitions ());
                    transitions.sort ((a, b) -> a.getInstant ().compareTo (b.getInstant ()));

                    Instant  switch_to_dst = null;
                    for (ZoneOffsetTransition transition : transitions) {
                        if (rules.isDaylightSavings (transition.getInstant ()) && !rules.isDaylightSavings (Instant.ofEpochSecond (transition.getInstant ().getEpochSecond () - 1))) {
                            switch_to_dst = transition.getInstant ();
                            break;
                        }
                    }

                    // I would give a warning, but this seems to be a frequent occasion.
                    // Maybe it's supposed to be like that.
                    if (switch_to_dst != null) {
                        ZonedDateTime  dst = ZonedDateTime.ofInstant (switch_to_dst, timezone);
                        ZonedDateTime  std = ZonedDateTime.ofInstant (Instant.ofEpochSecond (switch_to_dst.getEpochSecond () - 1), timezone);

                        names = new String[] { abbreviation_retriever.format (std),
                                               abbreviation_retriever.format (dst),
                                               full_name_retriever   .format (std),
                                               full_name_retriever   .format (dst) };

                        if (Objects.equals (names[0], names[1]) && Objects.equals (names[2], names[3])) {
                            // Another not quite understandable, but frequent thing.
                            // There seem to also be some timezone/locale pairs where
                            // abbreviations match, but full names don't.  Those we
                            // ignore.
                            names[1] = names[3] = null;
                        }
                    }
                }

                map.put (timezone.getId (), names);
            }

            data.put (locale, map);
        }

        for (Locale locale : getAllLocales ())
            removeUnnecessaryTimezoneNameData (data, locale);

        // Many timezones in Java are broken in that instants formatted/parsed in them get
        // shifted around.  Not much we can do about this, but at least we'll keep the
        // list internally so that we can avoid testing in them.
        List <String>      broken    = new ArrayList <> ();
        DateTimeFormatter  formatter = DateTimeFormatter.ofPattern ("yyyy-MM-dd HH:mm:ss z");
        Instant[]          check_at  = { Instant.from (formatter.parse ("2020-01-01 00:00:00 UTC")),
                                         Instant.from (formatter.parse ("2020-07-01 00:00:00 UTC")) };

        for (ZoneId timezone : getAllTimezones ()) {
            if (Arrays.stream (check_at)
                .anyMatch ((instant) -> !Objects.equals (instant, Instant.from (formatter.parse (formatter.format (ZonedDateTime.ofInstant (instant, timezone)))))))
                broken.add (timezone.getId ());
        }

        System.out.println ("(");

        for (Map.Entry <Locale, Map <String, String[]>> entry : data.entrySet ()) {
            Map <String, String>  values = new LinkedHashMap <> ();
            for (Map.Entry <String, String[]> name_entry : entry.getValue ().entrySet ()) {
                String[]  names = name_entry.getValue ();

                // See description of fallbacks in `datetime.el'.
                values.put (name_entry.getKey (), (names[0] != null || names[1] != null
                                                   ? (names[3] != null
                                                      ? String.format ("[%s %s %s %s]", quoteString (names[0]), quoteString (names[1]), quoteString (names[2]), quoteString (names[3]))
                                                      : String.format ("[%s %s]",       quoteString (names[0]),                         quoteString (names[2])))
                                                   : (names[3] != null
                                                      ? String.format ("(%s . %s)", quoteString (names[2]), quoteString (names[3]))
                                                      : quoteString (names[2]))));
            }

            System.out.println (toLispPlist (entry.getKey ().toLanguageTag (), values, false));
        }

        if (!broken.isEmpty ()) {
            broken.add (0, ":broken");
            System.out.println (toLispList (broken));
        }

        System.out.println (")");
    }

    protected static void removeUnnecessaryTimezoneNameData (Map <Locale, Map <String, String[]>> data, Locale locale)
    {
        if (Objects.equals (locale, Locale.ENGLISH))
            return;

        Map <String, String[]>  locale_data  = data.get (locale);
        Map <String, String[]>  english_data = data.get (Locale.ENGLISH);
        Locale                  parent       = new Locale (locale.getLanguage ());
        Map <String, String[]>  parent_data;

        if (Objects.equals (locale, parent))
            parent_data = english_data;
        else {
            removeUnnecessaryTimezoneNameData (data, parent);
            parent_data = data.get (parent);
        }

        // Discard abbreviated names that match those for English locale and leave only
        // the full names.
        locale_data.entrySet ().stream ().forEach ((entry) -> {
                String[]  names = entry.getValue ();

                // First clause is important for locales like `en-US', for example.
                if (!Arrays.equals (names, english_data.get (entry.getKey ()))
                    && Objects.equals (names[0], english_data.get (entry.getKey ()) [0]) && Objects.equals (names[1], english_data.get (entry.getKey ()) [1]))
                    names[0] = names[1] = null;
            });

        // Fall back to the parent locale where possible.
        locale_data.entrySet ().removeIf ((entry) -> Arrays.equals (entry.getValue (), parent_data.getOrDefault (entry.getKey (), english_data.get (entry.getKey ()))));
    }


    protected static String toLispList (List <?> list)
    {
        if (list == null || list.isEmpty ())
            return "nil";
        else
            return String.format ("(%s)", list.stream ().map (String::valueOf).collect (Collectors.joining (" ")));
    }

    protected static String toLispPlist (Map <String, String> properties, boolean quote_value_strings)
    {
        return toLispPlist (null, properties, quote_value_strings);
    }

    protected static String toLispPlist (String associate_to, Map <String, String> properties, boolean quote_value_strings)
    {
        return String.format ("(%s%s%s)",
                              associate_to != null ? associate_to : "",
                              associate_to != null && !properties.isEmpty () ? " " : "",
                              (properties.entrySet ().stream ()
                               .map ((entry) -> String.format ("%s %s", entry.getKey (), quote_value_strings ? quoteString (entry.getValue ()) : entry.getValue ()))
                               .collect (Collectors.joining (" "))));
    }

    protected static String toLispVector (List <String> strings)
    {
        return toLispVector (strings, true);
    }

    protected static String toLispVector (List <String> strings, boolean quote_value_strings)
    {
        return String.format ("[%s]", strings.stream ().map ((string) -> quote_value_strings ? quoteString (string) : string).collect (Collectors.joining (" ")));
    }

    protected static <K, V> String toLispAlist (Map <K, V> map, Function <? super K, String> key_to_string, Function <? super V, String> value_to_string)
    {
        return String.format ("(%s)",
                              map.entrySet ().stream ()
                              .map ((entry) -> String.format ("(%s . %s)", key_to_string.apply (entry.getKey ()), value_to_string.apply (entry.getValue ())))
                              .collect (Collectors.joining (" ")));
    }

    protected static Map <String, String> toPatternPlist (Function <FormatStyle, String> format)
    {
        Map <String, String>  patterns = new LinkedHashMap <> ();

        patterns.put (":short",  format.apply (FormatStyle.SHORT));
        patterns.put (":medium", format.apply (FormatStyle.MEDIUM));
        patterns.put (":long",   format.apply (FormatStyle.LONG));
        patterns.put (":full",   format.apply (FormatStyle.FULL));

        return patterns;
    }

    protected static String quoteString (String string)
    {
        return string != null ? String.format ("\"%s\"", string.replaceAll ("\\\\", "\\\\").replaceAll ("\"", "\\\"")) : "nil";
    }


    protected static boolean isLeapYear (int year)
    {
        return year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
    }
}
