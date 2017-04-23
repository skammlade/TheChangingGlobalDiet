----------------------------------------------------------------------------------
-------Calculate percent difference from global means by crop (ItemName)----------
----------------------------------------------------------------------------------

SELECT ItemName, ElementName, [Year], AVG([Value]) AS AvgValue
FROM dbo.QuickCropData
GROUP BY ItemName, ElementName, [Year]

SELECT original.CountryName, 
		original.ItemName, 
		original.ElementName, 
		original.[Year],
		original.FoodGroupName,
		--original.[Value], 
		--globalmeans.ItemName, 
		--globalmeans.ElementName, 
		--globalmeans.[Year], 
		globalmeans.AvgValue,
		((original.[Value] - globalmeans.AvgValue)/globalmeans.AvgValue) * 100 AS PercentDifferenceCrop
FROM dbo.QuickCropData original
	JOIN(
		SELECT ItemName, ElementName, [Year], AVG([Value]) AS AvgValue
		FROM dbo.QuickCropData
		GROUP BY ItemName, ElementName, [Year]) globalmeans on globalmeans.ItemName = original.ItemName 
															AND globalmeans.ElementName = original.ElementName
															AND globalmeans.[Year] = original.[Year]
WHERE globalmeans.AvgValue <> 0
											
