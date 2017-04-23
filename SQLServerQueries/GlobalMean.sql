USE [GlobalCropDiet]
GO

/****** Object:  View [dbo].[QuickCropData]    Script Date: 11/06/2016 21:35:08 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

--


Select * from [dbo].[QuickCropDataSummary] as (
SELECT 'Global Mean' [CountryName]
		--Item Name sometimes contains a comma which messes up CSV output.
		, case --WHEN ItemName LIKE '%other%' then 'Other'
			WHEN ItemName = 'Cereals, other' then 'Cereals (other)'
			WHEN ItemName = 'Citrus, other' then 'Citrus (other)'
			WHEN ItemName = 'Fruits, other' then 'Fruits (other)'
			WHEN ItemName = 'Oilcrops, other' then 'Oilcrops (other)'
			WHEN ItemName = 'Pulses, other' then 'Pulses (other)'
			WHEN ItemName = 'Roots, other' then 'Roots (other)'
			WHEN ItemName = 'Spices, other' then 'Spices (other)'
			WHEN ItemName = 'Sweeteners, other' then 'Sweeteners (other)'
			WHEN ItemName = 'Vegetables, other' then 'Vegetables (other)'
			WHEN ItemName = 'Barley' then 'Barley & beer'
			WHEN ItemName = 'Grapes' then 'Grapes & wine'
			WHEN ItemName = 'Beverages, alcoholic' then 'Alcoholic beverages'
			WHEN ItemName = 'Beverages, fermented' then 'Fermented beverages'
			ELSE ItemName
		END [ItemName]
		, FoodGroupName
		, ElementName
		, UnitName
		, [Year]
		, AVG([Value]) [Value]
FROM CropData cd
JOIN Item i on cd.ItemID=i.ItemID
JOIN FoodGroup fg on fg.FoodGroupID=i.FoodGroupID
JOIN Country c on c.CountryID=cd.CountryID
JOIN Element e on e.ElementID=cd.ElementID
JOIN Unit u on u.UnitID=cd.UnitID
--WHERE FoodGroupName<>'Miscellaneous'
GROUP BY ItemName, FoodGroupName, ElementName, UnitName, [Year]
UNION
/*SELECT 'Global Median' [CountryName]
		--Item Name sometimes contains a comma which messes up CSV output.
		, case --WHEN ItemName LIKE '%other%' then 'Other'
			WHEN ItemName = 'Cereals, other' then 'Cereals (other)'
			WHEN ItemName = 'Citrus, other' then 'Citrus (other)'
			WHEN ItemName = 'Fruits, other' then 'Fruits (other)'
			WHEN ItemName = 'Oilcrops, other' then 'Oilcrops (other)'
			WHEN ItemName = 'Pulses, other' then 'Pulses (other)'
			WHEN ItemName = 'Roots, other' then 'Roots (other)'
			WHEN ItemName = 'Spices, other' then 'Spices (other)'
			WHEN ItemName = 'Sweeteners, other' then 'Sweeteners (other)'
			WHEN ItemName = 'Vegetables, other' then 'Vegetables (other)'
			WHEN ItemName = 'Barley' then 'Barley & beer'
			WHEN ItemName = 'Grapes' then 'Grapes & wine'
			WHEN ItemName = 'Beverages, alcoholic' then 'Alcoholic beverages'
			WHEN ItemName = 'Beverages, fermented' then 'Fermented beverages'
			ELSE ItemName
		END [ItemName]
		, FoodGroupName
		, ElementName
		, UnitName
		, [Year]
		, AVG([Value]) [Value]
FROM CropData cd
JOIN Item i on cd.ItemID=i.ItemID
JOIN FoodGroup fg on fg.FoodGroupID=i.FoodGroupID
JOIN Country c on c.CountryID=cd.CountryID
JOIN Element e on e.ElementID=cd.ElementID
JOIN Unit u on u.UnitID=cd.UnitID
--WHERE FoodGroupName<>'Miscellaneous'
GROUP BY ItemName, FoodGroupName, ElementName, UnitName, [Year]
UNION */
SELECT 'Global Sum' [CountryName]
		--Item Name sometimes contains a comma which messes up CSV output.
		, case --WHEN ItemName LIKE '%other%' then 'Other'
			WHEN ItemName = 'Cereals, other' then 'Cereals (other)'
			WHEN ItemName = 'Citrus, other' then 'Citrus (other)'
			WHEN ItemName = 'Fruits, other' then 'Fruits (other)'
			WHEN ItemName = 'Oilcrops, other' then 'Oilcrops (other)'
			WHEN ItemName = 'Pulses, other' then 'Pulses (other)'
			WHEN ItemName = 'Roots, other' then 'Roots (other)'
			WHEN ItemName = 'Spices, other' then 'Spices (other)'
			WHEN ItemName = 'Sweeteners, other' then 'Sweeteners (other)'
			WHEN ItemName = 'Vegetables, other' then 'Vegetables (other)'
			WHEN ItemName = 'Barley' then 'Barley & beer'
			WHEN ItemName = 'Grapes' then 'Grapes & wine'
			WHEN ItemName = 'Beverages, alcoholic' then 'Alcoholic beverages'
			WHEN ItemName = 'Beverages, fermented' then 'Fermented beverages'
			ELSE ItemName
		END [ItemName]
		, FoodGroupName
		, ElementName
		, UnitName
		, [Year]
		, SUM([Value]) [Value]
FROM CropData cd
JOIN Item i on cd.ItemID=i.ItemID
JOIN FoodGroup fg on fg.FoodGroupID=i.FoodGroupID
JOIN Country c on c.CountryID=cd.CountryID
JOIN Element e on e.ElementID=cd.ElementID
JOIN Unit u on u.UnitID=cd.UnitID
--WHERE FoodGroupName<>'Miscellaneous'
GROUP BY ItemName, FoodGroupName, ElementName, UnitName, [Year]
)






GO


