import java.text.*;
import java.util.*;
import java.util.function.*;
import java.util.stream.*;


public class HarvestData
{
    public static void main (String[] args) throws Exception
    {
        List <Locale>  locales = new ArrayList <> (Arrays.asList (Locale.getAvailableLocales ()));
        locales.sort ((a, b) -> a.toLanguageTag ().compareToIgnoreCase (b.toLanguageTag ()));

        Map <Locale, Map <String, String>>  data = new LinkedHashMap <> ();

        List <String>  english_eras  = Arrays.asList (DateFormatSymbols.getInstance (Locale.ENGLISH).getEras ());
        List <String>  english_am_pm = Arrays.asList (DateFormatSymbols.getInstance (Locale.ENGLISH).getAmPmStrings ());

        for (Locale locale : locales) {
            Map <String, String>  map = new LinkedHashMap <> ();
            data.put (locale, map);

            if (DecimalFormatSymbols.getInstance (locale).getDecimalSeparator () != '.')
                map.put (":decimal-separator", String.format ("?%c", DecimalFormatSymbols.getInstance (locale).getDecimalSeparator ()));

            if (!Objects.equals (Arrays.asList (DateFormatSymbols.getInstance (locale).getEras ()), english_eras))
                map.put (":eras", toLispVector (Arrays.asList (DateFormatSymbols.getInstance (locale).getEras ())));

            map.put (":month-context-abbr",       toLispVector (getNames (locale, Calendar.MONTH,       Calendar.SHORT_FORMAT,     Calendar.JANUARY, Calendar.DECEMBER, -1)));
            map.put (":month-context-names",      toLispVector (getNames (locale, Calendar.MONTH,       Calendar.LONG_FORMAT,      Calendar.JANUARY, Calendar.DECEMBER, -1)));
            map.put (":weekday-context-abbr",     toLispVector (getNames (locale, Calendar.DAY_OF_WEEK, Calendar.SHORT_FORMAT,     Calendar.MONDAY,  Calendar.SATURDAY, Calendar.SUNDAY)));
            map.put (":weekday-context-names",    toLispVector (getNames (locale, Calendar.DAY_OF_WEEK, Calendar.LONG_FORMAT,      Calendar.MONDAY,  Calendar.SATURDAY, Calendar.SUNDAY)));
            map.put (":month-standalone-abbr",    toLispVector (getNames (locale, Calendar.MONTH,       Calendar.SHORT_STANDALONE, Calendar.JANUARY, Calendar.DECEMBER, -1)));
            map.put (":month-standalone-names",   toLispVector (getNames (locale, Calendar.MONTH,       Calendar.LONG_STANDALONE,  Calendar.JANUARY, Calendar.DECEMBER, -1)));
            map.put (":weekday-standalone-abbr",  toLispVector (getNames (locale, Calendar.DAY_OF_WEEK, Calendar.SHORT_STANDALONE, Calendar.MONDAY,  Calendar.SATURDAY, Calendar.SUNDAY)));
            map.put (":weekday-standalone-names", toLispVector (getNames (locale, Calendar.DAY_OF_WEEK, Calendar.LONG_STANDALONE,  Calendar.MONDAY,  Calendar.SATURDAY, Calendar.SUNDAY)));

            if (!Objects.equals (Arrays.asList (DateFormatSymbols.getInstance (locale).getAmPmStrings ()), english_am_pm))
                map.put (":am-pm", toLispVector (Arrays.asList (DateFormatSymbols.getInstance (locale).getAmPmStrings ())));

            removeUnnecessaryStandaloneStrings (map, ":month-standalone-abbr",    ":month-context-abbr");
            removeUnnecessaryStandaloneStrings (map, ":month-standalone-names",   ":month-context-names");
            removeUnnecessaryStandaloneStrings (map, ":weekday-standalone-abbr",  ":weekday-context-abbr");
            removeUnnecessaryStandaloneStrings (map, ":weekday-standalone-names", ":weekday-context-names");

            Map <String, String>  date_patterns = toPatternPlist ((style) -> (SimpleDateFormat) DateFormat.getDateInstance (style, locale));
            Map <String, String>  time_patterns = toPatternPlist ((style) -> (SimpleDateFormat) DateFormat.getTimeInstance (style, locale));

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

            boolean  date_part_first = true;
            String   separator       = null;

            for (int date_style : new int[] { DateFormat.SHORT, DateFormat.MEDIUM, DateFormat.LONG, DateFormat.FULL }) {
                for (int time_style : new int[] { DateFormat.SHORT, DateFormat.MEDIUM, DateFormat.LONG, DateFormat.FULL }) {
                    String  date_pattern      = ((SimpleDateFormat) DateFormat.getDateInstance (date_style, locale)).toPattern ();
                    String  time_pattern      = ((SimpleDateFormat) DateFormat.getTimeInstance (time_style, locale)).toPattern ();
                    String  date_time_pattern = ((SimpleDateFormat) DateFormat.getDateTimeInstance (date_style, time_style, locale)).toPattern ();

                    if (separator == null) {
                        boolean  found = false;

                        search_loop:
                        for (boolean date_part_first_ : new boolean[] { true, false }) {
                            for (String separator_ : new String[] { " ", ", " }) {
                                if (Objects.equals (date_time_pattern, String.format ("%s%s%s",
                                                                                      date_part_first_ ? date_pattern : time_pattern,
                                                                                      separator_,
                                                                                      date_part_first_ ? time_pattern : date_pattern))) {
                                    found           = true;
                                    date_part_first = date_part_first_;
                                    separator       = separator_;
                                    break search_loop;
                                }
                            }
                        }

                        if (!found) {
                            throw new IllegalStateException (String.format ("cannot determine separator:\n  locale: %s\n  date-time: %s\n  date: %s\n  time: %s",
                                                                            locale.toLanguageTag (), date_time_pattern, date_pattern, time_pattern));
                        }
                    }

                    if (!Objects.equals (date_time_pattern, String.format ("%s%s%s",
                                                                           date_part_first ? date_pattern : time_pattern,
                                                                           separator,
                                                                           date_part_first ? time_pattern : date_pattern))) {
                        throw new IllegalStateException (String.format ("unexpected date-time pattern:\n  locale: %s\n  date-time: %s\n  date: %s\n  time: %s",
                                                                        locale.toLanguageTag (), date_time_pattern, date_pattern, time_pattern));
                    }
                }
            }

            if (!date_part_first || !" ".equals (separator))
                map.put (":date-time-pattern-rule", String.format ("(%s . %s)", date_part_first ? "t" : "nil", quoteString (separator)));
        }

        // Remove duplicates.
        for (Locale locale : locales) {
            Locale  parent = new Locale (locale.getLanguage ());
            if (Objects.equals (locale, parent))
                continue;

            if (Objects.equals (data.get (parent), data.get (locale)))
                data.remove (locale);
            else {
                for (Iterator <Map.Entry <String, String>> it = data.get (locale).entrySet ().iterator (); it.hasNext ();) {
                    Map.Entry <String, String>  entry = it.next ();
                    if (Objects.equals (entry.getValue (), data.get (parent).get (entry.getKey ())))
                        it.remove ();
                }

                data.get (locale).put (":parent", parent.toLanguageTag ());
            }
        }

        System.out.println ("(");
        for (Map.Entry <Locale, Map <String, String>> entry : data.entrySet ())
            System.out.println (toLispPlist (entry.getKey ().toLanguageTag (), entry.getValue (), false));
        System.out.println (")");
    }

    protected static List <String> getNames (Locale locale, int field, int style, int from, int to, int extra)
    {
        Calendar       calendar = Calendar.getInstance (locale);
        List <String>  names    = new ArrayList <> ();

        for (int k = from; k <= to; k++) {
            calendar.set (field, k);
            names.add (calendar.getDisplayName (field, style, locale));

            // Java is not very consistent here, sometimes standalone strings are just null,
            // sometimes they duplicate context strings.
            if (names.get (names.size () - 1) == null)
                names.set (names.size () - 1, calendar.getDisplayName (field, style == Calendar.SHORT_STANDALONE ? Calendar.SHORT : Calendar.LONG, locale));
        }

        // Needed to put Sunday last.
        if (extra >= 0) {
            calendar.set (field, extra);
            names.add (calendar.getDisplayName (field, style, locale));

            if (names.get (names.size () - 1) == null)
                names.set (names.size () - 1, calendar.getDisplayName (field, style == Calendar.SHORT_STANDALONE ? Calendar.SHORT : Calendar.LONG, locale));
        }

        return names;
    }

    protected static void removeUnnecessaryStandaloneStrings (Map <String, String> properties, String standalone_key, String context_key)
    {
        if (Objects.equals (properties.get (standalone_key), properties.get (context_key)))
            properties.remove (standalone_key);
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
        return String.format ("[%s]", strings.stream ().map ((string) -> quoteString (string)).collect (Collectors.joining (" ")));
    }

    protected static Map <String, String> toPatternPlist (Function <Integer, SimpleDateFormat> format)
    {
        Map <String, String>  patterns = new LinkedHashMap <> ();

        patterns.put (":short",  format.apply (DateFormat.SHORT) .toPattern ());
        patterns.put (":medium", format.apply (DateFormat.MEDIUM).toPattern ());
        patterns.put (":long",   format.apply (DateFormat.LONG)  .toPattern ());
        patterns.put (":full",   format.apply (DateFormat.FULL)  .toPattern ());

        return patterns;
    }

    protected static String quoteString (String string)
    {
        return string != null ? String.format ("\"%s\"", string.replaceAll ("\\\\", "\\\\").replaceAll ("\"", "\\\"")) : "nil";
    }
}
