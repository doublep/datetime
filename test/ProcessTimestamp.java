import java.time.*;
import java.time.format.*;
import java.time.temporal.*;
import java.util.*;


public class ProcessTimestamp
{
    /**
     *  Usage (e.g.):
     *      echo format TIMESTAMP TIMEZONE LOCALE PATTERN | java ProcessTimestamp
     *  or:
     *      echo -e parse FORMATTED "\n" TIMEZONE LOCALE PATTERN | java ProcessTimestamp
     *
     *  where:
     *    TIMESTAMP is (only for command `format'): a double number of seconds since
     *      epoch time UTC;
     *    FORMATTED is (only for command `parse'): a timestamp formatted according to
     *      the parameters that follow; read until linefeed;
     *    TIMEZONE and LOCALE are string identifiers;
     *    PATTERN is according to SimpleDateFormat documentation and is taken until
     *      the end of line with starting and ending whitespace removed.
     *
     *  The five tokens can be repeated as many times as needed.  Output is either one
     *  formatted timestamp or one double number (depending on the command) per line,
     *  corresponding to each five tokens in the input.
     */
    public static void main (String[] args) throws Exception
    {
        Scanner  input = new Scanner (System.in).useLocale (Locale.ENGLISH);

        while (input.hasNext ()) {
            String  command = input.next ();
            if (!"format".equals (command) && !"parse".equals (command))
                throw new IllegalArgumentException (String.format ("unknown command '%s'", command));

            double  timestamp = ("format".equals (command) ? input.nextDouble ()       : 0.0);
            String  formatted = ("parse" .equals (command) ? input.nextLine ().trim () : null);
            ZoneId  timezone  = ZoneId.of (input.next ());
            Locale  locale    = Locale.forLanguageTag (input.next ());
            String  pattern   = input.nextLine ().trim ();

            switch (command) {
            case "format":
                System.out.println (DateTimeFormatter.ofPattern (pattern, locale)
                                    .format (LocalDateTime.ofInstant (Instant.ofEpochSecond ((long) Math.floor (timestamp),
                                                                                             (int) Math.floor ((timestamp - Math.floor (timestamp))  * 1_000_000_000)),
                                                                      timezone)));
                break;

            case "parse":
                DateTimeFormatterBuilder  builder = (new DateTimeFormatterBuilder ()
                                                     .parseCaseInsensitive ()
                                                     // Commented out since it triggers bugs in obscure locales in Java.
                                                     // We don't use this for testing anyway.
                                                     // .parseLenient ()  
                                                     .appendPattern (pattern));

                try {
                    // Apparently we cannot blindly set default values as they will
                    // conflict with actually parsed values (at least in some cases).
                    // This is not what I'd call defaults, but oh well...
                    TemporalAccessor  parsed = builder.toFormatter (locale).parse (formatted);

                    if (!parsed.isSupported (ChronoField.YEAR))
                        builder.parseDefaulting (ChronoField.YEAR, 1970);

                    if (!parsed.isSupported (ChronoField.MONTH_OF_YEAR))
                        builder.parseDefaulting (ChronoField.MONTH_OF_YEAR, 1);

                    if (!parsed.isSupported (ChronoField.DAY_OF_MONTH))
                        builder.parseDefaulting (ChronoField.DAY_OF_MONTH, 1);

                    if (!parsed.isSupported (ChronoField.HOUR_OF_DAY))
                        builder.parseDefaulting (ChronoField.HOUR_OF_DAY, 0);

                    if (!parsed.isSupported (ChronoField.MINUTE_OF_HOUR))
                        builder.parseDefaulting (ChronoField.MINUTE_OF_HOUR, 0);

                    if (!parsed.isSupported (ChronoField.SECOND_OF_MINUTE))
                        builder.parseDefaulting (ChronoField.SECOND_OF_MINUTE, 0);

                    if (!parsed.isSupported (ChronoField.NANO_OF_SECOND))
                        builder.parseDefaulting (ChronoField.NANO_OF_SECOND, 0);

                    Instant  result = (builder
                                       .toFormatter (locale).withZone (timezone)
                                       .parse (formatted, Instant::from));

                    long  seconds = result.getEpochSecond ();
                    int   nano    = result.getNano ();

                    if (seconds < 0 && nano != 0) {
                        seconds += 1;
                        nano     = 1_000_000_000 - nano;
                    }

                    System.out.println (String.format ("%d.%09d", seconds, nano).replaceFirst ("(?<!\\.)0*$", ""));
                }
                catch (DateTimeParseException exception) {
                    System.out.println ("nil");
                }

                break;
            }
        }
    }
}
