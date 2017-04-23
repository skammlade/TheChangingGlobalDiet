--Get counts by Country, Element & Year
select CountryName, ElementName, Year, count(*) as Count
from CropData cd
		JOIN Country c ON cd.CountryID = c.CountryID
		JOIN Element e ON cd.ElementID = e.ElementID
		JOIN Item i ON cd.ItemID = i.ItemID
WHERE Value <> 0
GROUP BY CountryName, ElementName, Year
order by CountryName, ElementName, Year


--Find records that have Food weight but no calories, or vice versa (switch > to < in the where clause)
select *
FROM
	(SELECT CountryName, year, elementname, count(*) cnt
	from CropData cd
			JOIN Country c ON cd.CountryID = c.CountryID
			JOIN Element e ON cd.ElementID = e.ElementID
			JOIN Item i ON cd.ItemID = i.ItemID
	WHERE value <> 0 AND cd.ElementID = 2
	group by CountryName, Year, elementname
	--order by CountryName, Year, elementname
	) CalCounts
JOIN 
	(SELECT CountryName, year, elementname, count(*) cnt
	from CropData cd
			JOIN Country c ON cd.CountryID = c.CountryID
			JOIN Element e ON cd.ElementID = e.ElementID
			JOIN Item i ON cd.ItemID = i.ItemID
	WHERE value <> 0 AND cd.ElementID = 4
	group by CountryName, Year, elementname
	) WeightCounts
ON CalCounts.CountryName = WeightCounts.CountryName AND calCounts.Year = WeightCounts.Year
WHERE CalCounts.Cnt < WeightCounts.Cnt


--Country/Year/Food for which cal = 0 and weight doesn't, or vice versa
select *
from 
(select countryname, year, itemname, value as CalValue
from CropData cd
		JOIN Country c ON cd.CountryID = c.CountryID
		JOIN Element e ON cd.ElementID = e.ElementID
		JOIN Item i ON cd.ItemID = i.ItemID
WHERE cd.elementid = 2) cals

left join 

(select  countryname, year, ItemName, value WeiValue
from CropData cd
		JOIN Country c ON cd.CountryID = c.CountryID
		JOIN Element e ON cd.ElementID = e.ElementID
		JOIN Item i ON cd.ItemID = i.ItemID
WHERE cd.elementid = 4) wei
on cals.countryname = wei.countryname AND cals.year = wei.year and cals.itemname = wei.itemname
WHERE (Cals.Calvalue = 0 AND wei.Weivalue <> 0) OR (Cals.Calvalue <> 0 AND wei.Weivalue = 0)