import java.time.*;
import java.time.format.*;
import java.util.*;


public class FormatTimestamp
{
    /**
     *  Usage (e.g.): echo TIMESTAMP TIMEZONE LOCALE PATTERN | java FormatTimestamp
     *
     *  where:
     *    TIMESTAMP is a double number of seconds since epoch time UTC;
     *    TIMEZONE and LOCALE are string identifiers;
     *    PATTERN is according to SimpleDateFormat documentation and is taken until
     *      the end of line with starting and ending whitespace removed.
     *
     *  The four tokens can be repeated as many times as needed.  Output is one formatted
     *  timestamp per line, corresponding to each quadruplet in the input.
     */
    public static void main (String[] args) throws Exception
    {
        Scanner  input = new Scanner (System.in).useLocale (Locale.ENGLISH);

        while (input.hasNext ()) {
            double  timestamp = input.nextDouble ();
            ZoneId  timezone  = ZoneId.of (input.next ());
            Locale  locale    = Locale.forLanguageTag (input.next ());
            String  pattern   = input.nextLine ().trim ();

            System.out.println (DateTimeFormatter.ofPattern (pattern, locale)
                                .format (LocalDateTime.ofInstant (Instant.ofEpochSecond ((long) Math.floor (timestamp),
                                                                                         (int) Math.floor ((timestamp - Math.floor (timestamp))  * 1_000_000_000)),
                                                                  timezone)));
        }
    }
}
