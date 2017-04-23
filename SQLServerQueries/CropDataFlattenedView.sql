SELECT top 100* 
FROM CropData

select top 100 *
from rawdata

SELECT top 1000000 CountryName, ItemName, FoodGroupName, ElementName, UnitName, [Year], [Value] 
FROM CropData cd
JOIN Item i on cd.ItemID=i.ItemID
JOIN FoodGroup fg on fg.FoodGroupID=i.FoodGroupID
JOIN Country c on c.CountryID=cd.CountryID
JOIN Element e on e.ElementID=cd.ElementID
JOIN Unit u on u.UnitID=cd.UnitID
WHERE [Year]=1988 and [Value]<>'0'
ORDER by [Value]

CREATE view QuickCropData as (
SELECT CountryName, ItemName, FoodGroupName, ElementName, UnitName, [Year], [Value] 
FROM CropData cd
JOIN Item i on cd.ItemID=i.ItemID
JOIN FoodGroup fg on fg.FoodGroupID=i.FoodGroupID
JOIN Country c on c.CountryID=cd.CountryID
JOIN Element e on e.ElementID=cd.ElementID
JOIN Unit u on u.UnitID=cd.UnitID
WHERE FoodGroupName<>'Miscellaneous'
)

SELECT TOP 10000* 
FROM QuickCropData
where CountryName like 'fra%' and ElementName='Food supply quantity'
ORDER by ItemName, [Year]

SELECT ItemName, avg(Value) as AvgValue, min(Value) as MinValue, max(Value) as MaxValue
FROM QuickCropData
where CountryName like 'fra%' and ElementName='Food supply quantity'
Group by ItemName
ORDER by ItemName