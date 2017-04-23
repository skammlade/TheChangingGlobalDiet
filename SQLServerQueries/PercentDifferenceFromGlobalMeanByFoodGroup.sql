---------------------------------------------------------------------------------------------
-------Calculate percent difference from global means by Food Group  (FoodGroupName)----------
---------------------------------------------------------------------------------------------

--Sum values for each country by foodgroup and year
SELECT CountryName, FoodGroupName, ElementName, [Year], SUM([Value]) AS SumValue
FROM dbo.QuickCropData
GROUP BY Countryname, FoodGroupName, ElementName, [Year]

--Subquery avg of sum values above
SELECT FoodGroupName, ElementName, [Year], avg(SumValue) AS AvgSumValue
FROM (
	SELECT CountryName, FoodGroupName, ElementName, [Year], SUM([Value]) AS SumValue
	FROM dbo.QuickCropData
	GROUP BY Countryname, FoodGroupName, ElementName, [Year]
	) totals
GROUP BY FoodGroupName, ElementName, [Year]

--Mainquery using two queries above
SELECT original.CountryName, 
		original.FoodGroupName, 
		original.ElementName, 
		original.[Year],
		--original.SumValue, 
		--totals.FoodGroupName, 
		--totals.ElementName, 
		--totals.[Year], 
		totals.AvgSumValue,
		((original.SumValue - totals.AvgSumValue)/totals.AvgSumValue) * 100 AS PercentDifferenceFoodGroup
FROM (
		SELECT CountryName, FoodGroupName, ElementName, [Year], SUM([Value]) AS SumValue
		FROM dbo.QuickCropData
		GROUP BY Countryname, FoodGroupName, ElementName, [Year]
		) original
	JOIN(
		SELECT FoodGroupName, ElementName, [Year], avg(SumValue) AS AvgSumValue
		FROM (
			SELECT CountryName, FoodGroupName, ElementName, [Year], SUM([Value]) AS SumValue
			FROM dbo.QuickCropData
			GROUP BY Countryname, FoodGroupName, ElementName, [Year]
			) totals
		GROUP BY FoodGroupName, ElementName, [Year]
		)	
		totals on totals.FoodGroupName = original.FoodGroupName 
				AND totals.ElementName = original.ElementName
				AND totals.[Year] = original.[Year]
WHERE totals.AvgSumValue <> 0
ORDER BY original.FoodGroupName, original.[Year], CountryName				




--Checking that each FoodGroup has the same number of Items
SELECT FoodGroupName, ElementName, stdev(ItemCount), avg(ItemCount), min(ItemCount), max(ItemCount)
FROM (
SELECT CountryName, FoodGroupName, ElementName, [Year], count(ItemName) AS ItemCount
FROM dbo.QuickCropData
--WHERE FoodGroupName='Grains' 
Group BY CountryName, FoodGroupName, ElementName, [Year]
--Having count(ItemName) <> 8
--ORDER BY FoodGroupName, count(ItemName) 
) AS A
GROUP BY FoodGroupName, ElementName
ORDER By FoodGroupName