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
    }

    protected static void printLocaleData () throws Exception
    {
        List <Locale>  locales = new ArrayList <> (Arrays.asList (Locale.getAvailableLocales ()));
        locales.sort ((a, b) -> a.toLanguageTag ().compareToIgnoreCase (b.toLanguageTag ()));

        Map <Locale, Map <String, String>>  data = new LinkedHashMap <> ();

        for (Locale locale : locales) {
            // This way we discard a few locales that can otherwise lead to duplicate keys
            // because of use of toLanguageTag().  E.g. `no_NO_NY' is problematic.
            if (locale.getVariant ().length () > 0)
                continue;

            Chronology  chronology = Chronology.ofLocale (locale);
            if (!chronology.getId ().equals ("ISO")) {
                // Ignore such locales for now.
                continue;
            }

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

            Boolean                      date_part_first = null;
            Map <List <String>, String>  separators      = new LinkedHashMap <> ();

            for (FormatStyle date_style : FormatStyle.values ()) {
                for (FormatStyle time_style : FormatStyle.values ()) {
                    String  date_pattern      = DateTimeFormatterBuilder.getLocalizedDateTimePattern (date_style, null,       chronology, locale);
                    String  time_pattern      = DateTimeFormatterBuilder.getLocalizedDateTimePattern (null,       time_style, chronology, locale);
                    String  date_time_pattern = DateTimeFormatterBuilder.getLocalizedDateTimePattern (date_style, time_style, chronology, locale);

                    if (date_part_first == null)
                        date_part_first = date_time_pattern.startsWith (date_pattern);

                    String  separator = null;

                    if (date_part_first && date_time_pattern.startsWith (date_pattern) && date_time_pattern.endsWith (time_pattern))
                        separator = date_time_pattern.substring (date_pattern.length (), date_time_pattern.length () - time_pattern.length ());
                    else if (!date_part_first && date_time_pattern.startsWith (time_pattern) && date_time_pattern.endsWith (date_pattern))
                        separator = date_time_pattern.substring (time_pattern.length (), date_time_pattern.length () - date_pattern.length ());
                    else {
                        throw new IllegalStateException (String.format ("cannot determine separator:\n  locale: %s\n  date-time: %s\n  date: %s\n  time: %s",
                                                                        locale.toLanguageTag (), date_time_pattern, date_pattern, time_pattern));
                    }

                    List <String>  key = new ArrayList <> ();
                    for (FormatStyle style : new FormatStyle[] { date_style, time_style })
                        key.add (style == FormatStyle.SHORT ? ":short" : style == FormatStyle.MEDIUM ? ":medium" : style == FormatStyle.LONG ? ":long" : ":full");

                    separators.put (key, separator);
                }
            }

            if (separators.values ().stream ().distinct ().collect (Collectors.counting ()) == 1)
                map.put (":date-time-pattern-rule", String.format ("(%s . %s)", date_part_first ? "t" : "nil", quoteString (separators.values ().stream ().distinct ().findFirst ().get ())));
            else
                map.put (":date-time-pattern-rule", String.format ("(%s . %s)", date_part_first ? "t" : "nil", toLispAlist (separators, (key) -> toLispList (key), (value) -> quoteString ((String) value))));
        }

        for (Locale locale : locales) {
            if (data.containsKey (locale))
                removeUnnecessaryLocaleData (data, locale);
        }

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

    protected static void removeUnnecessaryLocaleData (Map <Locale, Map <String, String>> data, Locale locale)
    {
        Map <String, String>  locale_data = data.get (locale);
        Locale                parent = new Locale (locale.getLanguage ());
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
        List <ZoneId>  timezones = ZoneId.getAvailableZoneIds ().stream ().map ((id) -> ZoneId.of (id)).collect (Collectors.toList ());
        timezones.sort ((a, b) -> a.getId ().compareToIgnoreCase (b.getId ()));

        Map <ZoneId, List <Object>>  data = new LinkedHashMap <> ();

        for (ZoneId timezone : timezones) {
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
                        System.err.println (String.format ("*Warning*: timezone '%s', offset transition at %s would be a potential rounding error", timezone.getId (), transition.getInstant ()));

                    while (year_offset >= transition_data.size ())
                        transition_data.add (new ArrayList <> (Arrays.asList (last_offset)));

                    transition_data.get (year_offset).add (transition.getInstant ().getEpochSecond () - (base + year_offset * AVERAGE_SECONDS_IN_YEAR));
                    transition_data.get (year_offset).add (last_offset = transition.getOffsetAfter ().getTotalSeconds ());
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
