
-- Let's see the tables

SELECT * FROM INFORMATION_SCHEMA.TABLES 

--Let's filter out the table_names

SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_TYPE = 'BASE TABLE'


--- Let's see the detailed view of the table

EXEC sp_columns all_hospitals

-- let's see the tables

select * from dbo.all_hospitals 
select * from dbo.census
select * from dbo.government_hospital
select * from dbo.housing 


/*

Problem Statement 19: (Verify upload)

Verification is a necessary process to be followed after uploading the data.
Join all the tables using the respective primary key- foreign key pair and show the top 3 rows. 
Verify whether the result is as expected or not. If not, then identify and fix the issue.

*/

select Top 3 *
from 
census c
join
housing h
on c.State_UT = h.State_UT
and c.District = h.District
left join
all_hospitals a
on c.State_UT = a.State_UT
left join
government_hospital g
on c.State_UT = g.State_UT


/*

Problem Statement 20: (Run Query on the database and use in python)

Write a query that finds the number of households without a toilet on the premise for each district of the state 
that has the least hospital bed-to-population ratio. Display the information using python.

*/

--Let's first get the state name where least hospitalbed-to-population ratio is present.

select top 1 *, round((HospitalBeds/Population)*100,4) as Hospitalbed_to_population
from
(
select c.State_UT,a.HospitalBeds , sum(Population) as population
from 
dbo.all_hospitals a
join
dbo.census c
on a.State_UT = c.State_UT
group by c.State_UT, HospitalBeds
) as m
order by Hospitalbed_to_population asc

-- Let's get the district wise details for number of no_toilet_on_premise for the state 'Bihar'
select State_UT, District, 
(Households_Rural + Households_Urban) - (Households_Rural_Toilet_Premise + Households_Urban_Toilet_Premise) as no_toilet_on_premise
from housing
where State_UT = 'Bihar'
order by no_toilet_on_premise desc


/*
Problem Statement 21: 
Create stored functions for the following:
*/

-- stored function :-  get_population_district

CREATE FUNCTION dbo.get_population_district(@District VARCHAR(255))
returns INT
as
begin
   declare @Population INT;

   select @Population = Population 
   from dbo.census
   where District = @District;

   RETURN @Population;

END;
GO

-- Let's check it
SELECT dbo.get_population_district('Kupwara') as Population

-- stored function :- get_population

CREATE FUNCTION dbo.get_population(@State_UT VARCHAR(255))
returns INT
as
begin
   declare @Population INT;

   select @Population = sum(Population) 
   from dbo.census
   where State_UT = @State_UT
   group by State_UT

   RETURN @Population;

END;

-- Let's check it
select dbo.get_population('Sikkim') as Population


-- stored function :- senior_citizen_population

CREATE FUNCTION dbo.senior_citizen_population(@State_UT VARCHAR(255))
RETURNS INT
AS 
BEGIN 
    DECLARE @Population INT;

	select @Population = sum(Senior_Citizen)
	from dbo.census
	where State_UT = @State_UT
	group by State_UT

	RETURN @Population;

end;

-- Let's check it
select dbo.senior_citizen_population('Andaman and Nicobar Islands') as Population


-- stored function :- get_hospital_beds

CREATE FUNCTION dbo.get_hospital_beds(@State_UT VARCHAR(255))
RETURNS INT
AS
BEGIN
     DECLARE @HospitalBeds INT;

	 SELECT @HospitalBeds = HospitalBeds
	 FROM 
	 dbo.all_hospitals
	 where State_UT = @State_UT

	 RETURN @HospitalBeds;
END;



-- Let's check it
select dbo.get_hospital_beds('Andhra Pradesh') as HospitalBeds


-- stored function :- get_govt_hospital_beds


CREATE FUNCTION dbo.get_govt_hospital_beds(@State_UT VARCHAR(255))
RETURNS INT
AS
BEGIN
     DECLARE @TotalHospitalBeds INT;

	 SELECT @TotalHospitalBeds = (Rural_Government_Beds + Urban_Government_Beds)
	 FROM 
	 dbo.government_hospital
	 where State_UT = @State_UT

	 RETURN @TotalHospitalBeds;
END;


-- Let's check it
select dbo.get_govt_hospital_beds('Arunachal Pradesh') as TotalGovtHospitalBeds

-- stored function :- beds_per_lakh  --Total number of Hospital beds per 1 lakh people

CREATE FUNCTION dbo.beds_per_lakh(@State_UT VARCHAR(255))
RETURNS INT
AS
BEGIN
     DECLARE @beds_per_1lakh INT;

	 select @beds_per_1lakh = round((HospitalBeds/population)*100000,0)
		from 
		(
		select 
		c.State_UT, sum(Population) as population, HospitalBeds
		from 
		census c
		join
		all_hospitals a
		on c.State_UT = a.State_UT
		where c.State_UT = @State_UT
		group by c.State_UT,HospitalBeds
		) n

	 RETURN @beds_per_1lakh;
END;

-- Let's check it
select dbo.beds_per_lakh('Maharashtra') as beds_per_1lakh



-- stored function :-  govt_beds_per_lakh   : -Total Number of government hospital beds per 1 lakh people

CREATE FUNCTION dbo.govt_beds_per_lakh(@State_UT VARCHAR(255))
RETURNS INT
AS
BEGIN
     DECLARE @govt_beds_per_lakh INT;

	select   @govt_beds_per_lakh = round((GovtHospitalBeds/population)*100000,0)
	from 
	(
	select 
	c.State_UT, sum(Population) as population, (Rural_Government_Beds + Urban_Government_Beds) as GovtHospitalBeds
	from 
	census c
	join
	government_hospital g

	on c.State_UT = g.State_UT
	where c.State_UT = @State_UT
	group by c.State_UT,Rural_Government_Beds,Urban_Government_Beds
	) n

	 RETURN @govt_beds_per_lakh;
END;


-- Let's check it
select dbo.govt_beds_per_lakh('Andaman and Nicobar Islands') as govt_beds_per_1lakh



/*

Problem Statement 22: 

It was reported* that in the north-eastern states, senior citizens are facing some issues in getting beds in a government hospital. 
Amir, from the department of healthcare, has requested a report on the healthcare situation in the north-eastern states.

The north-eastern states are listed in (Data/north_east_states.txt). Read the file and extract the state names using python 
and write a query to find the following information about those states from the database. 
Use the stored functions created earlier wherever relevant.

*	State Name
*	Population
*	Senior Citizen Population
*	Number of Government Hospital
*	Number of Government Hospital Beds
*	Number of Government Hospital beds for 1 Lakh population 
(Round the number to the nearest integer)
*	Number of Government Hospital beds for 1 Lakh senior citizen
(Round the number to the nearest integer)

The rows should be sorted by the number of Government Hospital beds for 1 Lakh senior citizens in ascending order.


*/

--- There is no north_east_states.txt file in the dataset provided

-- north_east_states = Arunachal Pradesh,Assam,Manipur,Meghalaya,Mizoram,Nagaland,Tripura,Sikkim



SELECT distinct
c.State_UT as State_Name, 
dbo.get_population(c.State_UT) AS Population,
dbo.senior_citizen_population(c.State_UT) AS senior_citizen_population,
(Rural_Government_Hospitals + Urban_Government_Hospitals) AS Number_of_Govt_Hospitals,
dbo.get_govt_hospital_beds(c.State_UT)  AS Govt_Hospital_Beds,
dbo.govt_beds_per_lakh(c.State_UT) AS Govt_Hospital_Beds_per_1lakh,
cast(round(dbo.get_govt_hospital_beds(c.State_UT)*100000.0 / dbo.senior_citizen_population(c.State_UT),0) as int) AS govt_beds_per_1lakh_senior_citizen
FROM 
census c
join
government_hospital g
on c.State_UT = g.State_UT
where c.State_UT in ('Arunachal Pradesh','Assam','Manipur','Meghalaya','Mizoram','Nagaland','Tripura','Sikkim')
order by govt_beds_per_1lakh_senior_citizen;


---Now we can create a view also by which we can access the data by the view name only---

CREATE VIEW North_east_senior_citizen_data
AS
SELECT distinct
c.State_UT as State_Name, 
dbo.get_population(c.State_UT) AS Population,
dbo.senior_citizen_population(c.State_UT) AS senior_citizen_population,
(Rural_Government_Hospitals + Urban_Government_Hospitals) AS Number_of_Govt_Hospitals,
dbo.get_govt_hospital_beds(c.State_UT)  AS Govt_Hospital_Beds,
dbo.govt_beds_per_lakh(c.State_UT) AS Govt_Hospital_Beds_per_1lakh,
cast(round(dbo.get_govt_hospital_beds(c.State_UT)*100000.0 / dbo.senior_citizen_population(c.State_UT),0) as int) AS govt_beds_per_1lakh_senior_citizen
FROM 
census c
join
government_hospital g
on c.State_UT = g.State_UT
where c.State_UT in ('Arunachal Pradesh','Assam','Manipur','Meghalaya','Mizoram','Nagaland','Tripura','Sikkim')

/*
Note : - 

the ORDER BY clause is not allowed in a view. If you need to order the data when querying the view, 
you can use the ORDER BY clause in your subsequent SELECT statement. 
Reason is when you write a view it is only the saved select query , so when we get the data then only we can
order it , thats why it throws error if you try to order.

*/
-- Let's check the  view
select * from North_east_senior_citizen_data order by govt_beds_per_1lakh_senior_citizen




/*

Problem Statement 23: 

The Government wants to run a scheme that would help people build in-premise toilets. 
For that purpose, a secretary has requested a district-wise report using the census data.

Using a stored procedure, Create a district-wise report that shows the total population of each district, 
the total number of households in the district, the number of households that do NOT have on-premise toilets, 
and the percentage of the number of households that do NOT toilets in premise to the total number of households. 
Sort the data in a way that is most useful for the secretary.

*/

CREATE PROCEDURE District_wise_report
as 
begin
	select c.State_UT,
	c.District, 
	dbo.get_population_district(c.District) as District_population,
	(h.Households_Rural + h.Households_Urban) as Total_Households,
	(h.Households_Rural + h.Households_Urban) - (Households_Rural_Toilet_Premise + Households_Urban_Toilet_Premise) as Households_without_toilet,
	round(((h.Households_Rural + h.Households_Urban) - (Households_Rural_Toilet_Premise + Households_Urban_Toilet_Premise))*100/(h.Households_Rural + h.Households_Urban),2)
	as Percentage_Households_without_toilet

	from
	census c
	join
	housing h
	on c.State_UT = h.State_UT
	and c.District = h.District
	order by Percentage_Households_without_toilet desc
end;

---Let's make use of this stored procedure to get the state wise data.
EXEC District_wise_report



/*

Problem Statement 24: 

An Agency wants to find out if there is a relationship between dilapidated homes and the lack of households for the 
people. Using a stored procedure, create a report that shows the name of each district, its population, the number 
of liveable houses per 1000 people, and the number of dilapidated households per 1000 people. Visualize the relation
between these values using an appropriate plot.


*/

CREATE PROCEDURE relationship_livable_dilapidated
as 
begin
select c.State_UT,
	c.District, 
	dbo.get_population_district(c.District) as District_population,
    round((h.Households_Rural_Livable + h.Households_Urban_Livable)*1000/dbo.get_population_district(c.District),0) as Livable_Households_Per_1000,
	round((h.Households_Rural_Dilapidated + h.Households_Urban_Dilapidated)*1000/dbo.get_population_district(c.District),0) as Dilapidated_Households_Per_1000
    from
	census c
	join
	housing h
	on c.State_UT = h.State_UT
	and c.District = h.District
end;

--drop PROCEDURE relationship_livable_dilapidated

---Let's make use of this stored procedure to get the households relationship data.
EXEC relationship_livable_dilapidated

-- we can access the stored procedure or function from the below line
select * from INFORMATION_SCHEMA.ROUTINES



/*

Problem Statement 25: 

The rural development department has requested a report on the healthcare situation in rural areas as compared to
urban areas. Write a query to find the number of hospital beds and government hospital beds per 1 lakh people for 
rural and urban areas separately for each state, along with the difference between them. 

*/



CREATE VIEW Rural_Urban_Comparison
AS
select *, abs(Rural_Beds_Per_1lakh - Urban_Beds_Per_1lakh) as Rural_Urban_Beds_Diff_1lakh
from
(
select State_UT,
dbo.beds_per_lakh(State_UT) as Number_of_Hospital_beds,
cast(round(Rural_Government_Beds*100000.0 /dbo.get_population(State_UT),0)as int) as Rural_Beds_Per_1lakh,
cast(round(Urban_Government_Beds*100000.0 /dbo.get_population(State_UT),0)as int) as Urban_Beds_Per_1lakh
from
government_hospital
) as d



--Let's see the data

SELECT * FROM Rural_Urban_Comparison order by Number_of_Hospital_beds desc;


/*
Problem Statement 26: 

New hospitals are under construction* in different locations, and once they are constructed the data in the 
database is to be updated. Also, when some hospitals non-operational hospital might be removed. Since the data 
is quite significant there should be a hospital_log table that would store any changes made to the hospital or 
government_hospital table.
The hospital_log table should contain the name of the district, whether the hospital is situated in urban or 
rural area, whether the hospital is being added or being removed, date of adding or removing the hospital to/from 
the existing facilities, and whether it is government hospital or not. Then a row in the table must be added automatically 
whenever a new hospital is added, or an existing hospital is removed. 

*/

-- Let's FirsT Create a Table

CREATE TABLE Hospital_Log(
State_UT VARCHAR(255),
urban_or_rural VARCHAR(255), CHECK (urban_or_rural IN ('urban', 'rural')),
added_or_removed VARCHAR(255), CHECK (added_or_removed IN ('added', 'removed')),
date_of_action date,
government_or_private VARCHAR(255), CHECK (government_or_private IN ('government', 'private'))
);


CREATE TRIGGER trg_government_hospital
ON dbo.government_hospital
AFTER UPDATE
AS 
BEGIN
        DECLARE @rural_urban VARCHAR(255);
        DECLARE @added_removed VARCHAR(255);

		IF UPDATE(Rural_Government_Hospitals)
		BEGIN
		   IF ((SELECT Rural_Government_Hospitals FROM inserted)- (SELECT Rural_Government_Hospitals FROM deleted)>0)
		      BEGIN
			     SET @rural_urban = 'rural';
				 SET @added_removed = 'added';
			  END;
		   ELSE 
		       BEGIN 
			      SET @rural_urban = 'rural';
				  SET @added_removed = 'removed';
			   END;
		    INSERT INTO Hospital_Log(State_UT,urban_or_rural,added_or_removed,date_of_action,government_or_private)
			SELECT State_UT,@rural_urban,@added_removed,GETDATE(),'government'
			from INSERTED;
		END;
		
		IF UPDATE(Urban_Government_Hospitals)
		BEGIN
		   IF ((SELECT Urban_Government_Hospitals FROM inserted)- (SELECT Urban_Government_Hospitals FROM deleted)>0)
		      BEGIN
			     SET @rural_urban = 'urban';
				 SET @added_removed = 'added';
			  END;
		   ELSE 
		       BEGIN 
			      SET @rural_urban = 'urban';
				  SET @added_removed = 'removed';
			   END;
		    INSERT INTO Hospital_Log(State_UT,urban_or_rural,added_or_removed,date_of_action,government_or_private)
			SELECT State_UT,@rural_urban,@added_removed,GETDATE(),'government'
			from INSERTED;
		END;
END;


-- Let's check  by updating the row
UPDATE dbo.government_hospital
set Rural_Government_Hospitals = 194
where State_UT = 'Andhra Pradesh'

-- let's check by deleting the row
UPDATE dbo.government_hospital
set Rural_Government_Hospitals = 193
where State_UT = 'Andhra Pradesh'


-- Let's check if the log is updated
select * from Hospital_Log


/*
Problem Statement 27:

New hospital beds are to be added in different government and private hospitals in different locations, 
and some non-operational hospital beds might be removed from hospitals. Since the data is quite significant there
should be a hospital_bed_log table that would store any changes made to the hospital or government_hospital table with
respect to number of hospital beds.
The hospital_bed_log table should contain the name of the State_UT, whether the hospital is situated in urban or 
rural area, date of adding or removing hospital beds, the number of beds being added or removed, whether it is being 
added or removed and whether it is added to/removed from government hospital or not. The table must be updated automatically
whenever new hospital beds are added to or removed from the existing facility.

*/



-- Let's FirsT Create a Table

CREATE TABLE hospital_bed_log(
State_UT VARCHAR(255),
urban_or_rural VARCHAR(255), CHECK (urban_or_rural IN ('urban', 'rural')),
date_of_action date,
added_or_removed VARCHAR(255), CHECK (added_or_removed IN ('added', 'removed')),
number_of_beds INT,
government_or_private VARCHAR(255), CHECK (government_or_private IN ('government', 'private'))
);




CREATE TRIGGER trg_government_hospital_bed
ON dbo.government_hospital
AFTER UPDATE
AS 
BEGIN
        DECLARE @rural_urban VARCHAR(255);
        DECLARE @added_removed VARCHAR(255);

		IF UPDATE(Rural_Government_Beds)
		BEGIN
		   IF ((SELECT Rural_Government_Beds FROM inserted)- (SELECT Rural_Government_Beds FROM deleted)>0)
		      BEGIN
			     SET @rural_urban = 'rural';
				 SET @added_removed = 'added';
			  END;
		   ELSE 
		       BEGIN 
			      SET @rural_urban = 'rural';
				  SET @added_removed = 'removed';
			   END;
		    INSERT INTO hospital_bed_log(State_UT,urban_or_rural,date_of_action,added_or_removed,number_of_beds,government_or_private)
			SELECT State_UT,@rural_urban,GETDATE(),@added_removed,
			(SELECT Rural_Government_Beds FROM inserted)- (SELECT Rural_Government_Beds FROM deleted),'government'
			from INSERTED;
		END;
		
		IF UPDATE(Urban_Government_Beds)
		BEGIN
		   IF ((SELECT Urban_Government_Beds FROM inserted)- (SELECT Urban_Government_Beds FROM deleted)>0)
		      BEGIN
			     SET @rural_urban = 'urban';
				 SET @added_removed = 'added';
			  END;
		   ELSE 
		       BEGIN 
			      SET @rural_urban = 'urban';
				  SET @added_removed = 'removed';
			   END;
		    INSERT INTO hospital_bed_log(State_UT,urban_or_rural,date_of_action,added_or_removed,number_of_beds,government_or_private)
			SELECT State_UT,@rural_urban,GETDATE(),@added_removed,
			(SELECT Urban_Government_Beds FROM inserted)- (SELECT Urban_Government_Beds FROM deleted),'government'
			from INSERTED;
		END;
END;



-- Let's check  by updating the row
UPDATE dbo.government_hospital
set Urban_Government_Beds = 16660
where State_UT = 'Andhra Pradesh'

-- let's check by deleting the row
UPDATE dbo.government_hospital
set Urban_Government_Beds = 16658
where State_UT = 'Andhra Pradesh'


-- Let's check if the log is updated
select * from hospital_bed_log






--ALL THE TABLES
select * from dbo.all_hospitals 
select * from dbo.census
select * from dbo.government_hospital
select * from dbo.housing 