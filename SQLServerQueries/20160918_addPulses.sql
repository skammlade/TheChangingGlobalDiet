--create new foodgroup
SELECT top 100 * 
FROM FoodGroup

INSERT INTO FoodGroup (FoodGroupName)
values ('Pulses')

--move food items to new food group
SELECT *
FROM Item

UPDATE Item
SET FoodGroupID=11
WHERE ItemID in (21, 33, 41)
--WHERE ItemID = 21 OR ItemID = 33 OR ItemID =  41

--change names of elements
UPDATE Element
SET ElementName='Fat'
WHERE ElementID=1

UPDATE Element
SET ElementName='Protein'
WHERE ElementID=3

UPDATE Element
SET ElementName='Fat'
WHERE ElementID=1