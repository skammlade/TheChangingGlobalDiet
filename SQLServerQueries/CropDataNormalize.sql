SELECT top 100 * 
FROM RawData

--populate Item table with distinct set of values from RawData
INSERT into Item (ItemName)
SELECT distinct Item 
FROM RawData

--join tables based on text field
--this should show a NULL itemID before the below update statement is run, and a populated ItemID after
SELECT *
FROM RawData rd join Item i on rd.Item=i.ItemName

--populate rawdata.itemID with the corresponding IDs from the Item table
UPDATE rd 
SET rd.ItemID=i.ItemID
FROM RawData rd join Item i on rd.Item=i.ItemName


SELECT distinct country
FROM RawData

--create a new table for country
--right click on a table -> script table as -> CREATE TO -> new query window
CREATE table Country (
	CountryID int IDENTITY(1,1) NOT NULL,
	CountryName varchar(50)
	CONSTRAINT PK_Country PRIMARY KEY CLUSTERED (CountryID)
)

--populate Country table with distinct set of values from RawData
INSERT into Country (CountryName)
SELECT distinct Country 
FROM RawData

--creat CountryID column in RawData table
ALTER table RawData 
ADD CountryID int


--populate rawdata.CountryID with the corresponding IDs from the Country table
UPDATE rd 
SET rd.CountryID=c.CountryID
FROM RawData rd join Country c on rd.Country=c.CountryName


SELECT distinct element,unit
FROM RawData

/**************************************************************************************
*****   Process Element Data                                                      *****
**************************************************************************************/

--create ElementID column in RawData table
ALTER table RawData 
ADD ElementID int

--create a new table for Element
--right click on a table -> script table as -> CREATE TO -> new query window
CREATE table Element (
	ElementID int IDENTITY(1,1) NOT NULL,
	ElementName varchar(50)
	CONSTRAINT PK_Element PRIMARY KEY CLUSTERED (ElementID)
)

--populate Country table with distinct set of values from RawData
INSERT into Element (ElementName)
SELECT distinct Element 
FROM RawData

--populate rawdata.ElementID with the corresponding IDs from the Element table
UPDATE rd 
SET rd.ElementID=e.ElementID
FROM RawData rd join Element e on rd.Element=e.ElementName


/**************************************************************************************
*****   Process Unit Data                                                      *****
**************************************************************************************/

--create UnitID column in RawData table
ALTER table RawData 
ADD UnitID int

--create a new table for Unit
--right click on a table -> script table as -> CREATE TO -> new query window
CREATE table Unit (
	UnitID int IDENTITY(1,1) NOT NULL,
	UnitName varchar(50)
	CONSTRAINT PK_Unit PRIMARY KEY CLUSTERED (UnitID)
)

--populate Country table with distinct set of values from RawData
INSERT into Unit (UnitName)
SELECT distinct Unit 
FROM RawData

--populate rawdata.UnitID with the corresponding IDs from the Unit table
UPDATE rd 
SET rd.UnitID=u.UnitID
FROM RawData rd join Unit u on rd.Unit=u.UnitName


/**************************************************************************************
*****   Process FoodGroup Data                                                    *****
**************************************************************************************/

--create FoodGroupID column in Item table
ALTER table Item
ADD FoodGroupID int

--create a new table for FoodGroup
--right click on a table -> script table as -> CREATE TO -> new query window
CREATE table FoodGroup (
	FoodGroupID int IDENTITY(1,1) NOT NULL,
	FoodGroupName varchar(50)
	CONSTRAINT PK_FoodGroup PRIMARY KEY CLUSTERED (FoodGroupID)
)

--populate FoodGroup table with distinct set of values from RawData_FoodGroup
INSERT into FoodGroup (FoodGroupName)
SELECT distinct Food_Group 
FROM RawData_FoodGroup

--populate Item.FoodGroupID with the corresponding IDs from the FoodGroup table
UPDATE i 
SET i.FoodGroupID=fg.FoodGroupID
--SELECT * 
FROM Item i join RawData_FoodGroup rdfg on i.ItemName=rdfg.Item
			join FoodGroup fg on rdfg.food_group=fg.FoodGroupName





SELECT distinct Food_Group from RawData_FoodGroup


SELECT * FROM FoodGroup
--SELECT * From Item
SELECT * From RawData_FoodGroup