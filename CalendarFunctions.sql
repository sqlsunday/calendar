--USE Calendar;
--GO
IF (SCHEMA_ID('Calendar') IS NULL) EXEC('CREATE SCHEMA [Calendar]');
GO
IF (OBJECT_ID('Calendar.Easter') IS NULL) EXEC('CREATE FUNCTION Calendar.Easter(@from date, @to date) RETURNS TABLE AS RETURN (SELECT NULL AS [null]);');
IF (OBJECT_ID('Calendar.Fiscal_annual') IS NULL) EXEC('CREATE FUNCTION Calendar.Fiscal_annual(@from date, @to date) RETURNS TABLE AS RETURN (SELECT NULL AS [null]);');
IF (OBJECT_ID('Calendar.Fiscal_4_4_5') IS NULL) EXEC('CREATE FUNCTION Calendar.Fiscal_4_4_5(@from date, @to date) RETURNS TABLE AS RETURN (SELECT NULL AS [null]);');
IF (OBJECT_ID('Calendar.Indian') IS NULL) EXEC('CREATE FUNCTION Calendar.Indian(@from date, @to date) RETURNS TABLE AS RETURN (SELECT NULL AS [null]);');
IF (OBJECT_ID('Calendar.Lunar_cycle') IS NULL) EXEC('CREATE FUNCTION Calendar.Lunar_cycle(@from date, @to date) RETURNS TABLE AS RETURN (SELECT NULL AS [null]);');
IF (OBJECT_ID('Calendar.Persian') IS NULL) EXEC('CREATE FUNCTION Calendar.Persian(@from date, @to date) RETURNS TABLE AS RETURN (SELECT NULL AS [null]);');
IF (OBJECT_ID('Calendar.Thai') IS NULL) EXEC('CREATE FUNCTION Calendar.Thai(@from date, @to date) RETURNS TABLE AS RETURN (SELECT NULL AS [null]);');
IF (OBJECT_ID('Calendar.Holidays') IS NULL) EXEC('CREATE FUNCTION Calendar.Holidays(@from date, @to date) RETURNS @return TABLE (i int NOT NULL) AS BEGIN; RETURN; END;');
IF (OBJECT_ID('Calendar.Sunrise_Sunset') IS NULL) EXEC('CREATE FUNCTION Calendar.Sunrise_Sunset(@from date, @to date) RETURNS TABLE AS RETURN (SELECT NULL AS [null]);');
IF (OBJECT_ID('Calendar.Dates') IS NULL) EXEC('CREATE FUNCTION Calendar.Dates(@from date, @to date) RETURNS TABLE AS RETURN (SELECT NULL AS [null]);');
GO
/*

Copyright Daniel Hutmacher under Creative Commons 4.0 license with attribution.
http://creativecommons.org/licenses/by/4.0/

Source: http://sqlsunday.com/downloads/

WHAT:       Returns a list of years, with western and orthodox
            Easter sundays for each year.

DISCLAIMER: This script may not be suitable to run in a production
            environment. I cannot assume any responsibility regarding
            the accuracy of the output information, performance
            impacts on your server, or any other consequence. If
            your juristiction does not allow for this kind of
            waiver/disclaimer, or if you do not accept these terms,
            you are NOT allowed to store, distribute or use this
            code in any way.

USAGE:      @from: Starting date
            @to:   Ending date

VERSION:    2018-07-17

*/

ALTER FUNCTION Calendar.Easter(@from date, @to date)
RETURNS TABLE
WITH SCHEMABINDING
AS

RETURN (
    --- Generic number table with 1000 rows:
    WITH n(i) AS (
        SELECT 0
        FROM        (VALUES (0), (1), (2), (3), (4), (5), (6), (7), (8), (9)) AS x1(i)
        CROSS APPLY (VALUES (0), (1), (2), (3), (4), (5), (6), (7), (8), (9)) AS x2(i)
        CROSS APPLY (VALUES (0), (1), (2), (3), (4), (5), (6), (7), (8), (9)) AS x3(i)),

    --- One row for each year between @from and @to:
    y([year]) AS (
        SELECT TOP (DATEDIFF(year, @from, @to)+1)
               YEAR(@from)+ROW_NUMBER() OVER (ORDER BY (SELECT NULL))-1
        FROM n AS x1, n AS x2
        )

    SELECT CAST(y.[year] AS smallint) AS [Year],
           w_easter7.easter_sunday AS Catholic_Easter_Sunday,
           e_easter6.easter_sunday AS Orthodox_Easter_Sunday
    FROM y

    --- Calculate Catholic Easter Sunday:
    CROSS APPLY (VALUES (y.[year]%19, FLOOR(1.0*y.[year]/100), y.[year]%100)) AS w_easter1(a, b, c)
    CROSS APPLY (VALUES (FLOOR(1.0*w_easter1.b/4), w_easter1.b%4, FLOOR((8.0+w_easter1.b)/25))) AS w_easter2(d, e, f)
    CROSS APPLY (VALUES (FLOOR((1.0+w_easter1.b-w_easter2.f)/3))) AS w_easter3(g)
    CROSS APPLY (VALUES ((19*w_easter1.a+w_easter1.b-w_easter2.d-w_easter3.g+15)%30, FLOOR(1.0*w_easter1.c/4), y.[year]%4)) AS w_easter4(h, i, k)
    CROSS APPLY (VALUES ((32.0+2*w_easter2.e+2*w_easter4.i-w_easter4.h-w_easter4.k)%7)) AS w_easter5(l)
    CROSS APPLY (VALUES (FLOOR((1.0*w_easter1.a+11*w_easter4.h+22*w_easter5.l)/451))) AS w_easter6(m)
    CROSS APPLY (VALUES (DATEFROMPARTS(y.[year], FLOOR((1.0*w_easter4.h+w_easter5.l-7*w_easter6.m+114)/31), (w_easter4.h+w_easter5.l-7*w_easter6.m+114)%31+1))) AS w_easter7(easter_sunday)

    --- Calculate orthodox Orthodox Easter Sunday
    CROSS APPLY (VALUES ((w_easter1.a*19+15)%30)) AS e_easter1(k)
    CROSS APPLY (VALUES ((w_easter4.k*2+y.[year]%7*4-e_easter1.k+34)%7+e_easter1.k+127)) AS e_easter2(e)
    CROSS APPLY (VALUES (FLOOR((1.0*e_easter2.e)/31), e_easter2.e%31)) AS e_easter3(m, d)
    CROSS APPLY (VALUES (e_easter3.d+(CASE WHEN e_easter3.m>4 THEN 1 ELSE 0 END)
                                +(CASE WHEN y.[year]>2099 THEN 1 ELSE 0 END))) AS e_easter4(d)
    CROSS APPLY (VALUES ((CASE WHEN e_easter4.d<30 THEN e_easter4.d+1 ELSE e_easter3.m+1-34+e_easter4.d END),
                            (CASE WHEN e_easter4.d<30 THEN e_easter3.m+1 ELSE e_easter3.m+2 END))) AS e_easter5(d, m)
    CROSS APPLY (VALUES (DATEFROMPARTS(y.[year], e_easter5.m-1, e_easter5.d))) AS e_easter6(easter_sunday)

    WHERE (w_easter7.easter_sunday<=@to   OR e_easter6.easter_sunday<=@to  ) AND
          (w_easter7.easter_sunday>=@from OR e_easter6.easter_sunday>=@from)
);

GO
/*

Copyright Daniel Hutmacher under Creative Commons 4.0 license with attribution.
http://creativecommons.org/licenses/by/4.0/

Source: http://sqlsunday.com/downloads/

WHAT:       Returns a gregorian calendar table for a specific interval of dates.

DISCLAIMER: This script may not be suitable to run in a production
            environment. I cannot assume any responsibility regarding
            the accuracy of the output information, performance
            impacts on your server, or any other consequence. If
            your juristiction does not allow for this kind of
            waiver/disclaimer, or if you do not accept these terms,
            you are NOT allowed to store, distribute or use this
            code in any way.

USAGE:      @from: Starting date
            @to:   Ending date

VERSION:    2018-09-02

*/

ALTER FUNCTION Calendar.Dates(@from date, @to date)
RETURNS TABLE
WITH SCHEMABINDING
AS

RETURN (
    --- Generic number table with 1000 rows:
    WITH n(i) AS (
        SELECT 0
        FROM        (VALUES (0), (1), (2), (3), (4), (5), (6), (7), (8), (9)) AS x1(i)
        CROSS APPLY (VALUES (0), (1), (2), (3), (4), (5), (6), (7), (8), (9)) AS x2(i)
        CROSS APPLY (VALUES (0), (1), (2), (3), (4), (5), (6), (7), (8), (9)) AS x3(i)),

    --- One row for each date between @from and @to.
    --- Uncomment x3 if you need more than 1 million days. :)
    n2(i) AS (
        SELECT TOP (DATEDIFF(day, @from, @to)+1)
               ROW_NUMBER() OVER (ORDER BY (SELECT NULL))-1
        FROM n AS x1, n AS x2 -- , n AS x3, n AS x4
        ),

    --- i is an incrementing integer, starting with 0 on @from.
    dt(i, [date]) AS(
        SELECT i, DATEADD(day, i, @from) AS [date]
        FROM n2)
    
    SELECT dt.i,
           dt.[date] AS [Date],
           CAST(YEAR(dt.[date]) AS smallint) AS [Year],
           CAST(DATEPART(quarter, dt.[date]) AS tinyint) AS [Quarter],
           CAST(CAST(YEAR(dt.[date]) AS char(4))+' Q'+CAST(DATEPART(quarter, dt.[date]) AS char(1)) AS char(7)) AS Year_quarter,
           CAST(MONTH(dt.[date]) AS tinyint) AS [Month],
           CAST(CAST(YEAR(dt.[date]) AS char(4))+'-'+REPLACE(STR(MONTH(dt.[date]), 2, 0), ' ', '0') AS char(7)) AS Year_month,
           CAST(DAY(dt.[date]) AS tinyint) AS [Day],
           CONVERT(char(10), dt.[date], 121) AS Date_ISO,

           CAST(dt1.iso_week_year AS smallint) AS ISO_week_year,
           CAST(DATEPART(isowk, dt.[date]) AS tinyint) AS [ISO_week],
           CAST(CAST(dt1.iso_week_year AS varchar(4))+' W'+REPLACE(STR(DATEPART(isowk, dt.[date]), 2, 0), ' ', '0') AS char(8)) AS ISO_year_week,
           CAST(dt1.weekday_iso AS tinyint) AS ISO_weekday_number,

           CAST(DATEPART(week, dt.[date]) AS tinyint) AS US_week,
           CAST(CAST(YEAR(dt.[date]) AS varchar(4))+' W'+REPLACE(STR(DATEPART(week, dt.[date]), 2, 0), ' ', '0') AS char(8)) AS US_year_week,
           CAST(dt1.weekday_us AS tinyint) AS US_weekday_number,

           DATENAME(weekday, dt.[date]) AS Weekday_name,
           CAST(DATEPART(dy, dt.[date]) AS smallint) AS Day_of_year,
           30*(MONTH(dt.[date])-1)+(CASE WHEN dt.[date]=EOMONTH(dt.[date]) THEN 30 ELSE DAY(dt.[date]) END) AS Day_of_year_30E_360,

           2451544.5+68.184/86400+DATEDIFF(day, {d '2000-01-01'}, dt.[date]) AS Julian_date

    FROM dt
    
    --- Calculation steps:
    CROSS APPLY (
        VALUES (
            1+(DATEPART(dw, dt.[date])+@@DATEFIRST-2)%7,
            1+(DATEPART(dw, dt.[date])+@@DATEFIRST-1)%7,
            YEAR(dt.[date])+(CASE WHEN DATEPART(isowk, dt.[date])>50 AND DATEPART(dy, dt.[date])<7 THEN -1
                                  WHEN DATEPART(isowk, dt.[date])=1  AND DATEPART(dy, dt.[date])>300 THEN 1
                                  ELSE 0 END)
        )) AS dt1(weekday_iso, weekday_us, iso_week_year)
    )

GO
/*

Copyright Daniel Hutmacher under Creative Commons 4.0 license with attribution.
http://creativecommons.org/licenses/by/4.0/

Source: http://sqlsunday.com/downloads/

WHAT:       Returns a fiscal 4-4-5-style or 52/53-style calendar
            between @from and @to.

DISCLAIMER: This script may not be suitable to run in a production
            environment. I cannot assume any responsibility regarding
            the accuracy of the output information, performance
            impacts on your server, or any other consequence. If
            your juristiction does not allow for this kind of
            waiver/disclaimer, or if you do not accept these terms,
            you are NOT allowed to store, distribute or use this
            code in any way.

USAGE:      @from:                  Starting date
            @to:                    Ending date
            @fiscal_year:           The first day of fiscal year 1.
                                    This date determines the starting
                                    weekday of a fiscal year.
            @weekly_style:          How many weeks each respective period
                                    of a quarter includes. Typically "445",
                                    but can sometimes be "544" or "454". The
                                    total must always be 13 weeks.
            @last_day_before_date:  1: fiscal year ends on last (weekday)
                                       before @fiscal_year.
                                    0: fiscal year ends on that (weekday)
                                       which is _closest_ to @fiscal_year.

VERSION:    2018-07-17

*/

ALTER FUNCTION Calendar.Fiscal_4_4_5 (
    @from                   date,
    @to                     date,
    @fiscal_year            date,
    @weekly_style           char(3)='445',
    @last_day_before_date   bit=1
)
RETURNS TABLE
AS

RETURN (
    --- A list of dates between @from and @to:
    WITH dt AS (
        SELECT i, [Date]
        FROM Calendar.Dates(@from, @to) AS dt)

    SELECT dt.i,
           dt.[Date],
           CAST(YEAR(x1.prev_date)-YEAR(@fiscal_year) AS smallint) AS Fiscal_year,
           CAST(x2.fiscal_year_name AS varchar(9)) AS Fiscal_year_name,
           x4.fiscal_year_start AS Fiscal_year_start,
           x4.fiscal_year_end AS Fiscal_year_end,

           CAST(dt5.[445_quarter] AS tinyint) AS Fiscal_quarter,
           CAST(x2.fiscal_year_name+' Q'+STR(dt5.[445_quarter], 1, 0) AS varchar(12)) AS Fiscal_year_quarter,

           CAST(dt4.[445_period] AS tinyint) AS Fiscal_period,
           CAST(x2.fiscal_year_name+' P'+REPLACE(STR(dt4.[445_period], 2, 0), ' ', '0') AS varchar(13)) AS Fiscal_year_period,

           CAST(dt3.[445_week] AS tinyint) AS Fiscal_week,
           CAST(x2.fiscal_year_name+' W'+REPLACE(STR(dt3.[445_week], 2, 0), ' ', '0') AS varchar(13)) AS Fiscal_year_week,

           CAST(dt1.day_of_445_year AS smallint) AS Day_of_fiscal_year,
           CAST(dt3.day_of_445_quarter AS tinyint) AS Day_of_fiscal_quarter,
           CAST(dt4.day_of_445_period AS tinyint) AS Day_of_fiscal_period
    FROM dt

    --- Calculation steps:
    CROSS APPLY (
        -- Nominal fiscal year start dates for the previous, current and two leading years:
        SELECT sub.[date] AS prev_date, LEAD(sub.[date], 1) OVER (ORDER BY sub.[date]) AS next_date
        FROM (
            VALUES (DATEFROMPARTS(YEAR(dt.[Date])-1, MONTH(@fiscal_year), DAY(@fiscal_year))),
                   (DATEFROMPARTS(YEAR(dt.[Date])  , MONTH(@fiscal_year), DAY(@fiscal_year))),
                   (DATEFROMPARTS(YEAR(dt.[Date])+1, MONTH(@fiscal_year), DAY(@fiscal_year))),
                   (DATEFROMPARTS(YEAR(dt.[Date])+2, MONTH(@fiscal_year), DAY(@fiscal_year)))
            ) AS sub([date])
        ) AS x1
    CROSS APPLY (
        -- Weekdays of the dates in x1:
        VALUES (1+(DATEPART(dw, x1.prev_date)+@@DATEFIRST-2)%7,
                1+(DATEPART(dw, x1.next_date)+@@DATEFIRST-2)%7,
                STR(YEAR(x1.prev_date), 4, 0)+(CASE WHEN MONTH(@fiscal_year)=1 THEN '' ELSE '/'+STR(YEAR(x1.prev_date)+1, 4, 0) END)
        )) AS x2(weekday_of_prev_date, weekday_of_next_date, fiscal_year_name)
    CROSS APPLY (
        -- From the nominal fiscal year start date, calculate the _actual_ last day
        -- of the fiscal year.
        VALUES (
           (CASE @last_day_before_date
            WHEN 1 THEN DATEADD(day, -(x2.weekday_of_prev_date)%7-1, x1.prev_date)
            WHEN 0 THEN DATEADD(day, -(x2.weekday_of_prev_date+3)%7+2, x1.prev_date) END),
           (CASE @last_day_before_date
            WHEN 1 THEN DATEADD(day, -(x2.weekday_of_next_date)%7-1, x1.next_date)
            WHEN 0 THEN DATEADD(day, -(x2.weekday_of_next_date+3)%7+2, x1.next_date) END)
        )) AS x3(last_day_of_fiscal_year, last_day_of_next_fiscal_year)
    CROSS APPLY (
        -- ... from which, we can determine the fiscal_year_stat and fiscal_year_end:
        SELECT DATEADD(day, 1, x3.last_day_of_fiscal_year) AS fiscal_year_start,
               x3.last_day_of_next_fiscal_year AS fiscal_year_end
        WHERE x3.last_day_of_fiscal_year<dt.[Date]
          AND x3.last_day_of_next_fiscal_year>=dt.[Date]
        ) AS x4
    CROSS APPLY (
        -- Break out @weekly_style into how many weeks go into the first,
        -- second and third period, as well as what day of the fiscal year
        -- this is:
        VALUES (
            TRY_CAST(LEFT(@weekly_style, 1) AS tinyint),
            TRY_CAST(SUBSTRING(@weekly_style, 2, 1) AS tinyint),
            TRY_CAST(SUBSTRING(@weekly_style, 3, 1) AS tinyint),
            1+DATEDIFF(day, x4.fiscal_year_start, dt.[Date])
        )) AS dt1([445_first], [445_second], [445_third], day_of_445_year)
    CROSS APPLY (
        -- From this, we can derive what day of the quarter we're in (with the
        -- possibility of a fifth quarter for leap weeks)...
        VALUES (
            1+(dt1.day_of_445_year-1)%(7*dt1.[445_first]+7*dt1.[445_second]+7*dt1.[445_third])
        )) AS dt2(day_of_445_quarter)
    CROSS APPLY (
        -- .. and correct for leap weeks:
        VALUES (
            (CASE WHEN dt1.day_of_445_year<=4*(7*dt1.[445_first]+7*dt1.[445_second]+7*dt1.[445_third])
                  THEN dt2.day_of_445_quarter
                  ELSE dt2.day_of_445_quarter+7*dt1.[445_first]+7*dt1.[445_second]+7*dt1.[445_third] END),
            (dt1.day_of_445_year-1)/7+1
        )) AS dt3(day_of_445_quarter, [445_week])
    CROSS APPLY (
        -- Now, we can calculate the period and the day of the period:
        VALUES (
            (CASE WHEN dt1.day_of_445_year<=  7*dt1.[445_first] THEN 1
                  WHEN dt1.day_of_445_year<=  7*dt1.[445_first]+  7*dt1.[445_second] THEN 2
                  WHEN dt1.day_of_445_year<=  7*dt1.[445_first]+  7*dt1.[445_second]+  7*dt1.[445_third] THEN 3
                  WHEN dt1.day_of_445_year<=2*7*dt1.[445_first]+1*7*dt1.[445_second]+1*7*dt1.[445_third] THEN 4
                  WHEN dt1.day_of_445_year<=2*7*dt1.[445_first]+2*7*dt1.[445_second]+1*7*dt1.[445_third] THEN 5
                  WHEN dt1.day_of_445_year<=2*7*dt1.[445_first]+2*7*dt1.[445_second]+2*7*dt1.[445_third] THEN 6
                  WHEN dt1.day_of_445_year<=3*7*dt1.[445_first]+2*7*dt1.[445_second]+2*7*dt1.[445_third] THEN 7
                  WHEN dt1.day_of_445_year<=3*7*dt1.[445_first]+3*7*dt1.[445_second]+2*7*dt1.[445_third] THEN 8
                  WHEN dt1.day_of_445_year<=3*7*dt1.[445_first]+3*7*dt1.[445_second]+3*7*dt1.[445_third] THEN 9
                  WHEN dt1.day_of_445_year<=4*7*dt1.[445_first]+3*7*dt1.[445_second]+3*7*dt1.[445_third] THEN 10
                  WHEN dt1.day_of_445_year<=4*7*dt1.[445_first]+4*7*dt1.[445_second]+3*7*dt1.[445_third] THEN 11
                  ELSE 12 END),
            dt3.day_of_445_quarter-(CASE
                  WHEN dt3.day_of_445_quarter<=7*dt1.[445_first] THEN 0
                  WHEN dt3.day_of_445_quarter<=7*dt1.[445_first]+7*dt1.[445_second] THEN 7*dt1.[445_first]
                  ELSE 7*dt1.[445_first]+7*dt1.[445_second] END)
        )) AS dt4([445_period], day_of_445_period)
    CROSS APPLY (
        -- .. and finally, the _actual_ quarter:
        VALUES (
            (dt4.[445_period]-1)/3+1
        )) AS dt5([445_quarter])
    );

GO
/*

Copyright Daniel Hutmacher under Creative Commons 4.0 license with attribution.
http://creativecommons.org/licenses/by/4.0/

Source: http://sqlsunday.com/downloads/

WHAT:       Returns a fiscal calendar between @from and @to, with
            a set fiscal year start date.
            Fiscal years are numbered sequentially, and if @fiscal_year
            is greater than the date, the fiscal year will be negative.

DISCLAIMER: This script may not be suitable to run in a production
            environment. I cannot assume any responsibility regarding
            the accuracy of the output information, performance
            impacts on your server, or any other consequence. If
            your juristiction does not allow for this kind of
            waiver/disclaimer, or if you do not accept these terms,
            you are NOT allowed to store, distribute or use this
            code in any way.

USAGE:      @from:          Starting date
            @to:            Ending date
            @fiscal_year:   Starting date of the fiscal year

VERSION:    2018-07-17

*/

ALTER FUNCTION Calendar.Fiscal_annual (
    @from                   date,
    @to                     date,
    @fiscal_year            date
)
RETURNS TABLE
AS

RETURN (
    --- Base table of dates between @from and @to:
    WITH dt AS (
        SELECT i, [Date]
        FROM Calendar.Dates(@from, @to))

    SELECT dt.i,
           dt.[Date],

           CAST(dt1.fiscal_year AS smallint) AS Fiscal_year,
           dt2.fiscal_year_name AS Fiscal_year_name,
           dt1.fiscal_year_start AS Fiscal_year_start,
           DATEADD(day, -1, DATEADD(year, 1, dt1.fiscal_year_start)) AS Fiscal_year_end,
           CAST(dt3.fiscal_quarter AS tinyint) AS Fiscal_quarter,
           CAST(dt2.fiscal_year_name+' Q'+CAST(dt3.fiscal_quarter AS char(1)) AS varchar(12)) AS Fiscal_year_quarter,
           CAST(dt2.fiscal_period AS tinyint) AS Fiscal_period,
           CAST(dt2.fiscal_year_name+' P'+REPLACE(STR(dt2.fiscal_period, 2, 0), ' ', '0') AS varchar(13)) AS Fiscal_year_period,
           CAST(1+DATEDIFF(week, DATEADD(day, 1-dt1.day_of_fiscal_year, dt.[Date]), dt.[Date]) AS tinyint) AS Fiscal_week,
           CAST(dt2.fiscal_year_name+' W'+CAST(1+DATEDIFF(week, DATEADD(day, 1-dt1.day_of_fiscal_year, dt.[Date]), dt.[Date]) AS varchar(2)) AS varchar(13)) AS Fiscal_year_week,
           CAST(dt1.day_of_fiscal_year AS smallint) AS Day_of_fiscal_year,
           CAST(1+DATEDIFF(day, DATEADD(month, 3*dt3.fiscal_quarter-3, dt1.fiscal_year_start), dt.[Date]) AS tinyint) AS Day_of_fiscal_quarter,
           CAST(1+DATEDIFF(day, DATEADD(month, (CASE WHEN DAY(@fiscal_year)>DAY(dt.[Date]) THEN -1 ELSE 0 END), DATEFROMPARTS(YEAR(dt.[Date]), MONTH(dt.[Date]), DAY(@fiscal_year))), dt.[Date]) AS tinyint) AS Day_of_fiscal_period
    FROM dt
    CROSS APPLY (
        -- Calculate the fiscal year and the day of the fiscal year:
        VALUES (
            DATEDIFF(year, @fiscal_year, dt.[Date])+(CASE
                WHEN 100*MONTH(@fiscal_year)+DAY(@fiscal_year)>
                     100*MONTH(dt.[Date])   +DAY(dt.[Date]) THEN 0 ELSE 1 END),
            1+DATEDIFF(day, DATEFROMPARTS(YEAR(dt.[Date])+(CASE
                WHEN 100*MONTH(@fiscal_year)+DAY(@fiscal_year)>
                     100*MONTH(dt.[Date])   +DAY(dt.[Date]) THEN -1 ELSE 0 END), MONTH(@fiscal_year), DAY(@fiscal_year)), dt.[Date]),
            DATEFROMPARTS(YEAR(dt.[Date])+(CASE
                WHEN 100*MONTH(@fiscal_year)+DAY(@fiscal_year)>
                     100*MONTH(dt.[Date])   +DAY(dt.[Date]) THEN -1 ELSE 0 END), MONTH(@fiscal_year), DAY(@fiscal_year))
        )) AS dt1(fiscal_year, day_of_fiscal_year, fiscal_year_start)
    CROSS APPLY (
        -- The fiscal year name is the calendar year name for fiscal years that start
        -- on January 1, otherwise it's (year-1)/(year). The fiscal period is a sequentially
        -- numbered month:
        VALUES (
            CAST((CASE WHEN MONTH(@fiscal_year)=1 THEN CAST(YEAR(dt.[Date]) AS varchar(9))
                       ELSE CAST(YEAR(@fiscal_year)+dt1.fiscal_year-1 AS varchar(9))+'/'+CAST(YEAR(@fiscal_year)+dt1.fiscal_year AS varchar(9)) END) AS varchar(9)),
            (12+MONTH(dt.[Date])-MONTH(@fiscal_year)+(CASE WHEN DAY(dt.[Date])<DAY(@fiscal_year) THEN -1 ELSE 0 END))%12+1
        )) AS dt2(fiscal_year_name, fiscal_period)
    CROSS APPLY (
        -- From the month, we also get the quarter (1-4)
        VALUES (
            (dt2.fiscal_period-1)/3+1
        )) AS dt3(fiscal_quarter)
    );

GO
/*

Copyright Daniel Hutmacher under Creative Commons 4.0 license with attribution.
http://creativecommons.org/licenses/by/4.0/

Source: http://sqlsunday.com/downloads/

WHAT:       Returns a calendar of Thai dates between @from and @to (gregorian).

DISCLAIMER: This script may not be suitable to run in a production
            environment. I cannot assume any responsibility regarding
            the accuracy of the output information, performance
            impacts on your server, or any other consequence. If
            your juristiction does not allow for this kind of
            waiver/disclaimer, or if you do not accept these terms,
            you are NOT allowed to store, distribute or use this
            code in any way.

USAGE:      @from: Starting date
            @to:   Ending date

VERSION:    2018-07-17

*/

ALTER FUNCTION Calendar.Thai (
    @from                   date,
    @to                     date
)
RETURNS TABLE
AS

RETURN (
    --- Base set of dates between @from and @to:
    WITH dt AS (
        SELECT i, [Date]
        FROM Calendar.Dates(@from, @to))

    SELECT dt.i,
           dt.[Date],
           CAST(dt1.thai_year AS smallint) AS Thai_year,
           dt1.thai_year_start AS Thai_year_start,
           CAST(dt1.thai_year_end AS date) AS Thai_year_end,
           CAST(dt2.thai_quarter AS tinyint) AS Thai_quarter,
           CAST(CAST(dt1.thai_year AS char(4))+' Q'+CAST(dt2.thai_quarter AS char(1)) AS char(7)) AS Thai_year_quarter,
           CAST(dt1.thai_month AS tinyint) AS Thai_month,
           m.long AS Thai_month_name,
           m.short AS Thai_month_name_short,
           CAST(CAST(dt1.thai_year AS char(4))+'-'+REPLACE(STR(dt1.thai_month, 2, 0), ' ', '0') AS char(7)) AS Thai_year_month,
           DATEFROMPARTS(YEAR(dt.[Date]), MONTH(dt.[Date]), 1) AS Thai_month_start,
           EOMONTH(dt.[Date]) AS Thai_month_end,
           DAY(dt.[Date]) AS Day_of_Thai_month,
           CAST(1+(DATEPART(dw, dt.[Date])+@@DATEFIRST-1)%7 AS tinyint) AS Weekday_number,
           CAST(1+DATEDIFF(day, dt1.thai_year_start, dt.[Date]) AS smallint) AS Day_of_Thai_year

    FROM dt
    CROSS APPLY (
        -- Calculate the Thai year..
        VALUES (
            (CASE WHEN dt.[Date]>={d '1941-01-01'} THEN YEAR(dt.[Date])+543
                  WHEN dt.[Date]>={d '1912-04-01'} AND MONTH(dt.[Date])>=4 THEN YEAR(dt.[Date])+543
                  WHEN dt.[Date]>={d '1912-04-01'} THEN YEAR(dt.[Date])+542
                  END),
            (CASE WHEN dt.[Date]>={d '1941-01-01'} THEN DATEFROMPARTS(YEAR(dt.[Date]), 1, 1)
                  WHEN dt.[Date]>={d '1912-04-01'} AND MONTH(dt.[Date])>=4 THEN DATEFROMPARTS(YEAR(dt.[Date]), 4, 1)
                  WHEN dt.[Date]>={d '1912-04-01'} THEN DATEFROMPARTS(YEAR(dt.[Date])-1, 4, 1)
                  END),
            (CASE WHEN dt.[Date]>={d '1941-01-01'} THEN DATEFROMPARTS(YEAR(dt.[Date]), 12, 31)
                  WHEN dt.[Date]>={d '1940-04-01'} THEN {d '1940-12-31'}
                  WHEN dt.[Date]>={d '1912-04-01'} AND MONTH(dt.[Date])>=4 THEN DATEFROMPARTS(YEAR(dt.[Date])+1, 3, 31)
                  WHEN dt.[Date]>={d '1912-04-01'} THEN DATEFROMPARTS(YEAR(dt.[Date]), 3, 31)
                  END),
            (CASE WHEN dt.[Date]>={d '1941-01-01'} THEN  MONTH(dt.[Date])
                  WHEN dt.[Date]>={d '1912-04-01'} THEN (MONTH(dt.[Date])+8)%12+1
                  END)
        )) AS dt1(thai_year, thai_year_start, thai_year_end, thai_month)
    CROSS APPLY (
        VALUES (
            1+(dt1.thai_month-1)/3
        )) AS dt2(thai_quarter)
    INNER JOIN (
        -- Full and abbreviated Thai names of months:
        VALUES ( 1, N'มกราคม', N'ม.ค.'),
               ( 2, N'กุมภาพันธ์', N'ก.พ.'),
               ( 3, N'มีนาคม', N'มี.ค.'),
               ( 4, N'เมษายน', N'เม.ย.'),
               ( 5, N'พฤษภาคม', N'พ.ค.'),
               ( 6, N'มิถุนายน', N'มิ.ย.'),
               ( 7, N'กรกฎาคม', N'ก.ค.'),
               ( 8, N'สิงหาคม', N'ส.ค.'),
               ( 9, N'กันยายน', N'ก.ย.'),
               (10, N'ตุลาคม', N'ต.ค.'),
               (11, N'พฤศจิกายน', N'พ.ย.'),
               (12, N'ธันวาคม', N'ธ.ค.')
        ) AS m([month], long, short) ON dt1.thai_month=m.[month]
    --- This function is probably not accurate before 1912 (CE)
    WHERE dt.[Date]>={d '1912-04-01'}
);

GO
/*

Copyright Daniel Hutmacher under Creative Commons 4.0 license with attribution.
http://creativecommons.org/licenses/by/4.0/

Source: http://sqlsunday.com/downloads/

WHAT:       Returns a calendar of Persian dates between @from and @to (gregorian).

CREDITS:    Based on: https://en.wikipedia.org/wiki/Solar_Hijri_calendar#Solar_Hijri_algorithmic_calendar

DISCLAIMER: This script may not be suitable to run in a production
            environment. I cannot assume any responsibility regarding
            the accuracy of the output information, performance
            impacts on your server, or any other consequence. If
            your juristiction does not allow for this kind of
            waiver/disclaimer, or if you do not accept these terms,
            you are NOT allowed to store, distribute or use this
            code in any way.

USAGE:      @from: Starting date
            @to:   Ending date

VERSION:    2018-07-17

*/

ALTER FUNCTION Calendar.Persian (
    @from                   date,
    @to                     date
)
RETURNS TABLE
AS

RETURN (
    -- Number table of 100 rows:
    WITH n(i) AS (
        SELECT 0
        FROM        (VALUES (0), (1), (2), (3), (4), (5), (6), (7), (8), (9)) AS x1(i)
        CROSS APPLY (VALUES (0), (1), (2), (3), (4), (5), (6), (7), (8), (9)) AS x2(i)
        ),

    -- More number tables..
    cycle AS (
        SELECT [year]
        FROM (
            VALUES (0), (1), (2), (3), (4), (5), (6),
                   (7), (8), (9), (10), (11), (12), (13),
                   (14), (15), (16), (17), (18), (19), (20),
                   (21), (22), (23), (24), (25), (26), (27),
                   (28), (29), (30), (31), (32)
            ) AS x([year])
            ),

    persian_year AS (
        SELECT 1308+ROW_NUMBER() OVER (ORDER BY ggc.ggc, gc.gc, c.[year]) AS persian_year,
               365+c.is_leap_year AS year_length,
               CAST(DATEADD(day, SUM(365+c.is_leap_year) OVER (ORDER BY ggc.ggc, gc.gc, c.[year] ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING), {d '1930-03-22'}) AS date) AS persian_year_start,
               c.is_leap_year
        FROM (
            --- 21 "grand grand cycles", consisting of...
            VALUES (0), (1), (2), (3), (4), (5), (6),
                   (7), (8), (9), (10), (11), (12), (13),
                   (14), (15), (16), (17), (18), (19), (20)
            ) AS ggc(ggc)
        CROSS APPLY (
            --- 4 "grand cycles", consisting of...
            VALUES (1, 29, 0), (2, 33, 29),
                   (3, 33, 62), (4, 33, 95)
            ) AS gc(gc, y_count, y_count_acc)
        CROSS APPLY (
            --- 29, 33, 33 and 33 years respectively:
            SELECT TOP (gc.y_count)
                   ggc.ggc*138+gc.y_count_acc+cycle.[year] AS [year],
                   (CASE WHEN cycle.[year]%4=0 AND cycle.[year]>0 THEN 1 ELSE 0 END) AS is_leap_year
            FROM cycle
            ORDER BY [year]
            ) AS c)

    SELECT DATEDIFF(day, @from, d.[date]) AS i,
           d.[date] AS [Date],
           CAST(py.persian_year AS smallint) AS Persian_year,
           py.persian_year_start AS Persian_year_start,
           DATEADD(day, 365+py.is_leap_year-1, py.persian_year_start) AS Persian_year_end,
           CAST((m.persian_month-1)/3+1 AS tinyint) AS Persian_quarter,
           CAST(STR(py.persian_year, 4, 0)+' Q'+CAST((m.persian_month-1)/3+1 AS char(1)) AS char(7)) AS Persian_year_quarter,
           CAST(m.persian_month AS tinyint) AS Persian_month,
           m2.Iranian_month_name,
           m2.Iranian_romanized_month_name,
           m2.Kurdish_month_name,
           m2.Afghan_Persian_month_name,
           m2.Afghan_Pashto_month_name,
           CAST(STR(py.persian_year, 4, 0)+'-'+REPLACE(STR(m.persian_month, 2, 0), ' ', '0') AS char(7)) AS Persian_year_month,
           DATEADD(day, m.offset, py.persian_year_start) AS Persian_month_start,
           DATEADD(day, m.offset+m.day_count-1, py.persian_year_start) AS Persian_month_end,
           CAST(1+DATEDIFF(day, DATEADD(day, m.offset, py.persian_year_start), d.[date]) AS tinyint) AS Day_of_Persian_month,
           CAST(1+(DATEPART(dw, d.[date])+@@DATEFIRST)%7 AS tinyint) AS Weekday_number,
           CAST(1+DATEDIFF(day, py.persian_year_start, d.[date]) AS smallint) AS Day_of_Persian_year
    FROM persian_year AS py
    CROSS APPLY (
        --- Split each year into 12 months:
        VALUES (1, 0, 31), (2, 31, 31), (3, 62, 31),
               (4, 93, 31), (5, 124, 31), (6, 155, 31),
               (7, 186, 30), (8, 216, 30), (9, 246, 30),
               (10, 276, 30), (11, 306, 30), (12, 336, 29+py.is_leap_year)
        ) AS m(persian_month, offset, day_count)
    INNER JOIN (
        --- (and give them names of months, in different regional formats)
        VALUES
            ( 1, N'فروردین ',       N'Farvardin ',  N'خاکەلێوە‎ ', N'حمل ', N'وری '),
            ( 2, N'اردیبهشت ',      N'Ordibehesht ', N'گوڵان‎ ',    N'ثور ', N'غويی '),
            ( 3, N'خرداد ',         N'Khordad ',    N'جۆزەردان‎ ', N'جوزا ', N'غبرګولی '),
            ( 4, N'تیر ',           N'Tir ',        N'پووشپەڕ‎ ',  N'سرطان ', N'چنګاښ '),
            ( 5, N'امرداد) مرداد) ', N'Mordad ',    N'گەلاوێژ‎ ',   N'اسد ', N'زمری '),
            ( 6, N'شهریور ',        N'Shahrivar ',  N'خەرمانان‎ ', N'سنبله ', N'وږی '),
            ( 7, N'مهر ',           N'Mehr ',       N'ڕەزبەر‎ ',   N'میزان ', N'تله '),
            ( 8, N'آبان ',          N'Aban ',       N'گەڵاڕێزان‎ ', N'عقرب ', N'لړم '),
            ( 9, N'آذر ',           N'Azar ',       N'سەرماوەز‎ ', N'قوس ', N'ليندۍ '),
            (10, N'دی ',            N'Dey ',        N'بەفرانبار‎ ', N'جدی ', N'مرغومی '),
            (11, N'بهمن ',          N'Bahman ',     N'ڕێبەندان‎ ',  N'دلو ', N'سلواغه '),
            (12, N'اسفند ',         N'Esfand ',     N'ڕەشەمە‎ ',    N'حوت ', N'كب ')
        ) AS m2(persian_month, Iranian_month_name, Iranian_romanized_month_name, Kurdish_month_name, Afghan_Persian_month_name, Afghan_Pashto_month_name)
            ON m.persian_month=m2.persian_month
    CROSS APPLY (
        --- ... and split each month into days:
        SELECT TOP (m.day_count)
               DATEADD(day, ROW_NUMBER() OVER (ORDER BY (SELECT NULL))+m.offset-1, py.persian_year_start) AS [date]
        FROM n
        ) AS d

    -- Not sure if this calendar is accurate after 1478 (2099 CE)
    WHERE py.persian_year<=1478
      AND d.[date] BETWEEN @from AND @to)

GO
/*

Copyright Daniel Hutmacher under Creative Commons 4.0 license with attribution.
http://creativecommons.org/licenses/by/4.0/

Source: http://sqlsunday.com/downloads/

WHAT:       Returns a calendar of Indian (Shalivahana Shaka) dates
            between @from and @to (gregorian).

DISCLAIMER: This script may not be suitable to run in a production
            environment. I cannot assume any responsibility regarding
            the accuracy of the output information, performance
            impacts on your server, or any other consequence. If
            your juristiction does not allow for this kind of
            waiver/disclaimer, or if you do not accept these terms,
            you are NOT allowed to store, distribute or use this
            code in any way.

USAGE:      @from: Starting date
            @to:   Ending date

VERSION:    2018-07-17

*/

ALTER FUNCTION Calendar.Indian (
    @from                   date,
    @to                     date
)
RETURNS TABLE
AS

RETURN (
    --- Base table of dates between @from and @to:
    WITH dt AS (
        SELECT i, [Date]
        FROM Calendar.Dates(@from, @to))

    SELECT dt.i,
           dt.[Date],
           CAST(dt5.shaka_year AS smallint) AS Indian_year,
           dt4.shaka_year_start AS Indian_year_start,
           dt4.shaka_year_end AS Indian_year_end,
           CAST(dt6.shaka_quarter AS tinyint) AS Indian_quarter,
           CAST(STR(dt5.shaka_year, 4, 0)+' Q'+STR(dt6.shaka_quarter, 1, 0) AS char(7)) AS Indian_year_quarter,
           CAST(dt5.shaka_month AS tinyint) AS Indian_month,
           m.[name] AS Indian_month_name,
           CAST(STR(dt5.shaka_year, 4, 0)+'-'+REPLACE(STR(dt5.shaka_month, 2, 0), ' ', '0') AS char(7)) AS Indian_year_month,
           dt5.start_of_shaka_month AS Indian_month_start,
           (CASE WHEN dt5.shaka_month>=7 THEN DATEADD(day, 29, dt5.start_of_shaka_month)
                 WHEN dt5.shaka_month>=2 THEN DATEADD(day, 30, dt5.start_of_shaka_month)
                 WHEN dt5.shaka_month =1 THEN DATEFROMPARTS(YEAR(dt.[Date]), 4, 20)
                 END) AS Indian_month_end,
           CAST(1+DATEDIFF(day, dt5.start_of_shaka_month, dt.[Date]) AS tinyint) AS Day_of_Indian_month,
           CAST(1+(DATEPART(dw, dt.[Date])+@@DATEFIRST-1)%7 AS tinyint) AS Weekday_number,
           CAST(dt5.day_of_shaka_year AS smallint) AS Day_of_Indian_year
    FROM dt
    CROSS APPLY (
        -- Starting and ending date of Indian year (excluding leap years)
        VALUES (
            DATEADD(day, 22, DATEFROMPARTS(YEAR(dt.[Date])-1, 2, 28)),
            DATEADD(day, 22, DATEFROMPARTS(YEAR(dt.[Date])  , 2, 28)),
            DATEADD(day, 21, DATEFROMPARTS(YEAR(dt.[Date])+1, 2, 28))
        )) AS dt3(previous_shaka_year_start, shaka_year_start, shaka_year_end)
    CROSS APPLY (
        -- If the CE date is before the start of the Indian year, subtract 1 year
        -- from the Indian year starting and ending dates:
        VALUES (
            (CASE WHEN dt.[Date]<dt3.shaka_year_start THEN dt3.previous_shaka_year_start ELSE dt3.shaka_year_start END),
            (CASE WHEN dt.[Date]<dt3.shaka_year_start THEN DATEADD(day, -1, dt3.shaka_year_start) ELSE dt3.shaka_year_end END)
        )) AS dt4(shaka_year_start, shaka_year_end)
    CROSS APPLY (
        -- Calculate the current year and month,
        -- as well as the start date of the month and year:
        VALUES (
            YEAR(dt4.shaka_year_start)-78,
            (CASE WHEN 100*MONTH(dt.[Date])+DAY(dt.[Date])>=1222 THEN 10
                  WHEN 100*MONTH(dt.[Date])+DAY(dt.[Date])>=1122 THEN  9
                  WHEN 100*MONTH(dt.[Date])+DAY(dt.[Date])>=1023 THEN  8
                  WHEN 100*MONTH(dt.[Date])+DAY(dt.[Date])>= 923 THEN  7
                  WHEN 100*MONTH(dt.[Date])+DAY(dt.[Date])>= 823 THEN  6
                  WHEN 100*MONTH(dt.[Date])+DAY(dt.[Date])>= 723 THEN  5
                  WHEN 100*MONTH(dt.[Date])+DAY(dt.[Date])>= 622 THEN  4
                  WHEN 100*MONTH(dt.[Date])+DAY(dt.[Date])>= 522 THEN  3
                  WHEN 100*MONTH(dt.[Date])+DAY(dt.[Date])>= 421 THEN  2
                  WHEN DATEDIFF(day, dt4.shaka_year_start, dt.[Date])<=31 THEN  1
                  WHEN 100*MONTH(dt.[Date])+DAY(dt.[Date])>= 220 THEN 12
                  WHEN 100*MONTH(dt.[Date])+DAY(dt.[Date])>= 121 THEN 11
                  ELSE 10 END),
            (CASE WHEN 100*MONTH(dt.[Date])+DAY(dt.[Date])>=1222 THEN DATEFROMPARTS(YEAR(dt.[Date]),   12, 22)
                  WHEN 100*MONTH(dt.[Date])+DAY(dt.[Date])>=1122 THEN DATEFROMPARTS(YEAR(dt.[Date]),   11, 22)
                  WHEN 100*MONTH(dt.[Date])+DAY(dt.[Date])>=1023 THEN DATEFROMPARTS(YEAR(dt.[Date]),   10, 23)
                  WHEN 100*MONTH(dt.[Date])+DAY(dt.[Date])>= 923 THEN DATEFROMPARTS(YEAR(dt.[Date]),    9, 23)
                  WHEN 100*MONTH(dt.[Date])+DAY(dt.[Date])>= 823 THEN DATEFROMPARTS(YEAR(dt.[Date]),    8, 23)
                  WHEN 100*MONTH(dt.[Date])+DAY(dt.[Date])>= 723 THEN DATEFROMPARTS(YEAR(dt.[Date]),    7, 23)
                  WHEN 100*MONTH(dt.[Date])+DAY(dt.[Date])>= 622 THEN DATEFROMPARTS(YEAR(dt.[Date]),    6, 22)
                  WHEN 100*MONTH(dt.[Date])+DAY(dt.[Date])>= 522 THEN DATEFROMPARTS(YEAR(dt.[Date]),    5, 22)
                  WHEN 100*MONTH(dt.[Date])+DAY(dt.[Date])>= 421 THEN DATEFROMPARTS(YEAR(dt.[Date]),    4, 21)
                  WHEN DATEDIFF(day, dt4.shaka_year_start, dt.[Date])<=31 THEN dt4.shaka_year_start    
                  WHEN 100*MONTH(dt.[Date])+DAY(dt.[Date])>= 220 THEN DATEFROMPARTS(YEAR(dt.[Date]),    2, 20)
                  WHEN 100*MONTH(dt.[Date])+DAY(dt.[Date])>= 121 THEN DATEFROMPARTS(YEAR(dt.[Date]),    1, 21)
                                                                 ELSE DATEFROMPARTS(YEAR(dt.[Date])-1, 12, 22) END),
            1+DATEDIFF(day, dt4.shaka_year_start, dt.[Date])
        )) AS dt5(shaka_year, shaka_month, start_of_shaka_month, day_of_shaka_year)
    CROSS APPLY (
        -- The quarter is derived from the month number:
        VALUES (
            (dt5.shaka_month-1)/3+1
        )) AS dt6(shaka_quarter)
    LEFT JOIN (
        -- .. and add Indian month names:
        VALUES ( 1, N'Chaitra'),
               ( 2, N'Vaishākha'),
               ( 3, N'Jyēshtha'),
               ( 4, N'Āshādha'),
               ( 5, N'Shrāvana'),
               ( 6, N'Bhaadra'),
               ( 7, N'Āshwin'),
               ( 8, N'Kārtika'),
               ( 9, N'Agrahayana'),
               (10, N'Pausha'),
               (11, N'Māgha'),
               (12, N'Phalguna')
        ) AS m([month], [name]) ON dt5.shaka_month=m.[month]
    --- Official start date of the Indian national calendar:
    WHERE dt.[Date]>={d '1957-03-22'}

);

GO
/*

Copyright Daniel Hutmacher under Creative Commons 4.0 license with attribution.
http://creativecommons.org/licenses/by/4.0/

Source: http://sqlsunday.com/downloads/

WHAT:       Returns a calendar between @from and @to with calculated
            lunar cycles, which may be useful for some lunar-related
            date calculations (such as easter and lunar-based calendars).
            The first day of the lunar cycle corresponds to a new moon.

CREDITS:    Based on: https://www.daniweb.com/programming/software-development/code/453788/moon-phase-at-a-given-date-python

DISCLAIMER: This script may not be suitable to run in a production
            environment. I cannot assume any responsibility regarding
            the accuracy of the output information, performance
            impacts on your server, or any other consequence. If
            your juristiction does not allow for this kind of
            waiver/disclaimer, or if you do not accept these terms,
            you are NOT allowed to store, distribute or use this
            code in any way.

USAGE:      @from: Starting date
            @to:   Ending date

VERSION:    2018-07-17

*/

ALTER FUNCTION Calendar.Lunar_cycle (
    @from                   date,
    @to                     date
)
RETURNS TABLE
AS

RETURN (
    --- Base table of dates between @from and @to:
    WITH dt AS (
        SELECT i, [Date]
        FROM Calendar.Dates(@from, @to)),

    cal AS (
        SELECT dt.i,
               dt.[Date],
               dt2.days_into_phase,
               (CASE WHEN LAG(dt2.days_into_phase, 1, 99) OVER (ORDER BY dt.i)>dt2.days_into_phase THEN i END) AS new
        FROM dt
        INNER JOIN (
            -- Offsets, per month:
            VALUES (1, -1), (2, 1), (3, 0), (4, 1), (5, 2), (6, 3), (7, 4), (8, 5), (9, 7), (10, 7), (11, 9), (12, 9)
            ) AS m([month], month_offset) ON MONTH(dt.[Date])=m.[month]
        INNER JOIN (
            -- Age, corresponding to (year+1)%19
            VALUES (1, 18), (2, 0), (3, 11), (4, 22), (5, 3), (6, 14), (7, 25), (8, 6), (9, 17), (10, 28),
                   (11, 9), (12, 20), (13, 1), (14, 12), (15, 23), (16, 4), (17, 15), (18, 26), (19, 7)
            ) AS a(age_no, age) ON a.age_no=(YEAR(dt.[Date])+1)%19+1
        CROSS APPLY (
            -- How many days into the current lunar phase we are:
            -- NB: this number is not entirely sequential, and not
            --     always zero- or one-based.
            VALUES (
                ((a.age+
                    (((CASE WHEN DAY(dt.[Date])=31 THEN 1 ELSE DAY(dt.[Date]) END)+m.month_offset)%30)+
                    (CASE WHEN YEAR(dt.[Date])<1900 THEN 1 ELSE 0 END)
                    )%30)
            )) AS dt2(days_into_phase))

    SELECT i,
           [Date],
           DATEADD(day, MAX(new) OVER (ORDER BY i ROWS UNBOUNDED PRECEDING)-i, [Date]) AS Lunar_cycle_start,
           CAST(COUNT(new) OVER (ORDER BY i ROWS UNBOUNDED PRECEDING) AS smallint) AS Lunar_cycle,
           1+i-MAX(new) OVER (ORDER BY i ROWS UNBOUNDED PRECEDING) AS Day_of_lunar_cycle,
           (CASE CAST((days_into_phase + 2) * 16./59. AS int)
                 WHEN 0 THEN 'new (totally dark)'
                 WHEN 1 THEN 'waxing crescent (increasing to full)'
                 WHEN 2 THEN 'in its first quarter (increasing to full)'
                 WHEN 3 THEN 'waxing gibbous (increasing to full)'
                 WHEN 4 THEN 'full (full light)'
                 WHEN 5 THEN 'waning gibbous (decreasing from full)'
                 WHEN 6 THEN 'in its last quarter (decreasing from full)'
                        ELSE 'waning crescent (decreasing from full)'
                        END) AS Phase_description
    FROM cal)

GO
/*

Copyright Daniel Hutmacher under Creative Commons 4.0 license with attribution.
http://creativecommons.org/licenses/by/4.0/

Source: http://sqlsunday.com/downloads/

WHAT:       Returns the UTC time of sunrise and sunset, according to
            the formulas on https://en.wikipedia.org/wiki/Sunrise_equation

DISCLAIMER: This script may not be suitable to run in a production
            environment. I cannot assume any responsibility regarding
            the accuracy of the output information, performance
            impacts on your server, or any other consequence. If
            your juristiction does not allow for this kind of
            waiver/disclaimer, or if you do not accept these terms,
            you are NOT allowed to store, distribute or use this
            code in any way.

USAGE:      @from:                  Starting date
            @to:                    Ending date
            @west_long:             Location, degrees east (positive) or west (negative)
            @north_lat:             Location, degrees north (positive) or south (negative)
            @altitude:              Location, altitude in meters above sea level

VERSION:    2018-09-02

*/

ALTER FUNCTION Calendar.Sunrise_Sunset (
    @from                   date,
    @to                     date,
    @west_long              float,
    @north_lat              float,
    @altitude               float=0
)
RETURNS TABLE
AS

RETURN (
    --- A list of dates between @from and @to:
    WITH dt AS (
        SELECT i, [Date],
               CAST([Date] AS datetime2(0)) AS Date_datetime2
        FROM Calendar.Dates(@from, @to) AS dt)

    SELECT dt.i,
           dt.[Date],
           CAST(DEGREES(x6.[δ]) AS numeric(6, 3)) AS Declination,
           CAST(DEGREES(x8.[ω0]) AS numeric(6, 3)) AS Hour_angle,
           x10.Sunrise_UTC,
           x10.Solar_noon_UTC,
           x10.Sunset_UTC
    FROM dt
    CROSS APPLY (
        --- Mean solar noon (approximate) at @west_long:
        SELECT [J*]=1.*DATEDIFF(day, {d '2000-01-01'}, dt.[Date])+0.5-@west_long/360
        ) AS x2
    CROSS APPLY (
        --- Solar mean anomaly:
        SELECT M=RADIANS(CAST(357.5291+0.98560028*x2.[J*] AS numeric(38, 30))%360)
        ) AS x3
    CROSS APPLY (
        --- Equation of the center
        SELECT C=1.9148*SIN(x3.M)+0.02*SIN(2.*x3.M)+0.0003*SIN(3.*x3.M)
        ) AS x4
    CROSS APPLY (
        --- Ecliptic longitude
        SELECT [λ]=RADIANS(CAST(DEGREES(x3.M)+x4.C+180+102.9372 AS numeric(38, 30))%360)
        ) AS x5
    CROSS APPLY (
        --- Solar transit:
        SELECT J_transit=/* 2451545.5+ */ x2.[J*]+0.0053*SIN(x3.M)-0.0069*SIN(2.*x5.[λ]),
        --- Declination:
               [δ]=ASIN(SIN(x5.[λ])*SIN(RADIANS(23.44))),
               altitude_offset=-0.03461*(CASE WHEN @altitude>=0 THEN SQRT(@altitude) ELSE 0.0 END)
        ) AS x6
    CROSS APPLY (
        --- Hour angle:
        SELECT [ω0]=ACOS((SIN(RADIANS(-0.83+x6.altitude_offset))-SIN(RADIANS(@north_lat))*SIN(x6.[δ]))
                   /(COS(RADIANS(@north_lat))*COS(x6.[δ])))
        ) AS x8
    CROSS APPLY (
        --- "Julian" (actually, day-offset of) sunrise, solar noon and sunset:
        SELECT J_Sunrise=x6.J_transit-0.5*x8.[ω0]/PI(),
               J_Solar_noon=x6.J_transit,
               J_Sunset=x6.J_transit+0.5*x8.[ω0]/PI()
        ) AS x9
    CROSS APPLY (
        SELECT Sunrise_UTC=DATEADD(second, 3600*24*(x9.J_Sunrise), {d '2000-01-01'}),
               Solar_noon_UTC=DATEADD(second, 3600*24*(x9.J_Solar_noon), {d '2000-01-01'}),
               Sunset_UTC=DATEADD(second, 3600*24*(x9.J_Sunset), {d '2000-01-01'})
        ) AS x10
    );

GO
