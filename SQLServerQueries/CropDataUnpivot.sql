
SELECT * FROM RawData


--the long way to unpivot
SELECT ItemID, CountryID, ElementID, UnitID, 1961 as [year], y1961 as [value]
FROM RawData

UNION
SELECT ItemID, CountryID, ElementID, UnitID, 1962 as [year], y1962 as [value]
FROM RawData




--the hack way to unpivot
SELECT 'UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, '+ right (name,4) +' as [year], '+ name + ' as [value] FROM RawData'
From sys.columns 
WHERE name like 'Y%'

--output from above hack query, remove UNION from first line
SELECT ItemID, CountryID, ElementID, UnitID, 1961 as [year], Y1961 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1962 as [year], Y1962 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1963 as [year], Y1963 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1964 as [year], Y1964 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1965 as [year], Y1965 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1966 as [year], Y1966 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1967 as [year], Y1967 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1968 as [year], Y1968 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1969 as [year], Y1969 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1970 as [year], Y1970 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1971 as [year], Y1971 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1972 as [year], Y1972 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1973 as [year], Y1973 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1974 as [year], Y1974 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1975 as [year], Y1975 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1976 as [year], Y1976 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1977 as [year], Y1977 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1978 as [year], Y1978 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1979 as [year], Y1979 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1980 as [year], Y1980 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1981 as [year], Y1981 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1982 as [year], Y1982 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1983 as [year], Y1983 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1984 as [year], Y1984 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1985 as [year], Y1985 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1986 as [year], Y1986 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1987 as [year], Y1987 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1988 as [year], Y1988 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1989 as [year], Y1989 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1990 as [year], Y1990 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1991 as [year], Y1991 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1992 as [year], Y1992 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1993 as [year], Y1993 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1994 as [year], Y1994 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1995 as [year], Y1995 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1996 as [year], Y1996 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1997 as [year], Y1997 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1998 as [year], Y1998 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 1999 as [year], Y1999 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 2000 as [year], Y2000 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 2001 as [year], Y2001 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 2002 as [year], Y2002 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 2003 as [year], Y2003 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 2004 as [year], Y2004 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 2005 as [year], Y2005 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 2006 as [year], Y2006 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 2007 as [year], Y2007 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 2008 as [year], Y2008 as [value] FROM RawData
UNION ALL SELECT ItemID, CountryID, ElementID, UnitID, 2009 as [year], Y2009 as [value] FROM RawData
ORDER by [year]


--unpivot using UNPIVOT
SELECT ItemID, CountryID, ElementID, UnitID, right([year],4) as [Year], [Value] 
INTO CropData
FROM  

(SELECT [Item]
      ,[ItemID]
      ,[Country]
      ,[Region]
      ,[Element]
      ,[Unit]
      ,[Y1961]
      ,[Y1962]
      ,[Y1963]
      ,[Y1964]
      ,[Y1965]
      ,[Y1966]
      ,[Y1967]
      ,[Y1968]
      ,[Y1969]
      ,[Y1970]
      ,[Y1971]
      ,[Y1972]
      ,[Y1973]
      ,[Y1974]
      ,[Y1975]
      ,[Y1976]
      ,[Y1977]
      ,[Y1978]
      ,[Y1979]
      ,[Y1980]
      ,[Y1981]
      ,[Y1982]
      ,[Y1983]
      ,[Y1984]
      ,[Y1985]
      ,[Y1986]
      ,[Y1987]
      ,[Y1988]
      ,[Y1989]
      ,[Y1990]
      ,[Y1991]
      ,[Y1992]
      ,[Y1993]
      ,[Y1994]
      ,[Y1995]
      ,[Y1996]
      ,[Y1997]
      ,[Y1998]
      ,[Y1999]
      ,[Y2000]
      ,[Y2001]
      ,[Y2002]
      ,[Y2003]
      ,[Y2004]
      ,[Y2005]
      ,[Y2006]
      ,[Y2007]
      ,[Y2008]
      ,[Y2009]
      ,[CountryID]
      ,[ElementID]
      ,[UnitID]
  FROM [GlobalCropDiet].[dbo].[RawData]
  ) as rd
  
UNPIVOT 
(
value for [year] in (
	   [Y1961]
      ,[Y1962]
      ,[Y1963]
      ,[Y1964]
      ,[Y1965]
      ,[Y1966]
      ,[Y1967]
      ,[Y1968]
      ,[Y1969]
      ,[Y1970]
      ,[Y1971]
      ,[Y1972]
      ,[Y1973]
      ,[Y1974]
      ,[Y1975]
      ,[Y1976]
      ,[Y1977]
      ,[Y1978]
      ,[Y1979]
      ,[Y1980]
      ,[Y1981]
      ,[Y1982]
      ,[Y1983]
      ,[Y1984]
      ,[Y1985]
      ,[Y1986]
      ,[Y1987]
      ,[Y1988]
      ,[Y1989]
      ,[Y1990]
      ,[Y1991]
      ,[Y1992]
      ,[Y1993]
      ,[Y1994]
      ,[Y1995]
      ,[Y1996]
      ,[Y1997]
      ,[Y1998]
      ,[Y1999]
      ,[Y2000]
      ,[Y2001]
      ,[Y2002]
      ,[Y2003]
      ,[Y2004]
      ,[Y2005]
      ,[Y2006]
      ,[Y2007]
      ,[Y2008]
      ,[Y2009]
      )) as up