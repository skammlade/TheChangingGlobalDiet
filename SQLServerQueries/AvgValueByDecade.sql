-------------------------------------------------------------------
-------------Avg Value by Decade for NMDS-------------------------
--------------------------------------------------------------------


SELECT CountryName, ItemName, ElementName, Decade, AVG([Value]) AS AvgValueForDecade
From (
	SELECT CountryName, FoodGroupName, ItemName, ElementName, [Year], (([Year]/10)*10) AS Decade, [Value]
	FROM dbo.QuickCropData
	) DataWithDecades
--Where CountryName='Peru' AND ElementName='Calories' AND ItemName='Rye'
Group By CountryName, ItemName, ElementName, Decade

