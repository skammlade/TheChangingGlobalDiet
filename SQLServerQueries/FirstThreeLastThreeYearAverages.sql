SELECT firstgroup.ItemName, firstgroup.ElementName, firstgroup.UnitName, firstgroup.AvgValue61to63, lastgroup.AvgValue07to09
From

(SELECT ItemName, ElementName, UnitName, AVG([Value]) AS AvgValue61to63
FROM dbo.QuickCropData
WHERE ElementName='Calories' and Year<='1963'
GROUP BY ItemName, ElementName, UnitName) firstgroup

JOIN

(SELECT ItemName, ElementName, UnitName, AVG([Value]) AS AvgValue07to09
FROM dbo.QuickCropData
WHERE ElementName='Calories' and Year>='2007'
GROUP BY ItemName, ElementName, UnitName) lastgroup

ON firstgroup.ItemName=lastgroup.ItemName





Select top 50 * from QuickCropData