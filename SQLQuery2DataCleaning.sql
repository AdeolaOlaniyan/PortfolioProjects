/****** Script for SelectTopNRows command from SSMS  ******/
--Cleaning Data
Select *
From PortfolioProjectSql.dbo.NashvilleHousing

Select SaleDate
From PortfolioProjectSql.dbo.NashvilleHousing

--Standardize Date Format
Select SaleDate, CONVERT(Date, SaleDate) as SalesDateConverted
From PortfolioProjectSql.dbo.NashvilleHousing

-- Populate Prpperty Address
Select *
From PortfolioProjectSql.dbo.NashvilleHousing
Where PropertyAddress is null

Select *
From PortfolioProjectSql.dbo.NashvilleHousing
Order by  ParcelID

Select *
From PortfolioProjectSql.dbo.NashvilleHousing a
Join PortfolioProjectSql.dbo.NashvilleHousing b
    on a.ParcelID = b.ParcelID
	AND a.[UniqueID ] <> b.[UniqueID ]
Where a.PropertyAddress is null

Update a
SET PropertyAddress = ISNULL(a.PropertyAddress, b.PropertyAddress)
From PortfolioProjectSql.dbo.NashvilleHousing a
Join PortfolioProjectSql.dbo.NashvilleHousing b
    on a.ParcelID = b.ParcelID
	AND a.[UniqueID ] <> b.[UniqueID ]
Where a.PropertyAddress is null

--Breaking out Address into Individual Columns (Address, City, State)

Select PropertyAddress
From PortfolioProjectSql.dbo.NashvilleHousing

Select 
SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress) -1) as Address
, SUBSTRING(PropertyAddress, CHARINDEX(',', PropertyAddress) +1, LEN(PropertyAddress)) as Address
From PortfolioProjectSql.dbo.NashvilleHousing

Alter Table NashvilleHousing
Add PropertySplitAddress Nvarchar(255);

Update NashvilleHousing
Set PropertySplitAddress = SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress) -1)

Alter Table NashvilleHousing
Add PropertySplitCity Nvarchar(255);

Update NashvilleHousing
Set PropertySplitCity = SUBSTRING(PropertyAddress, CHARINDEX(',', PropertyAddress) +1, LEN(PropertyAddress)) as Address

Select *
From PortfolioProjectSql.dbo.NashvilleHousing

Select OwnerAddress
From PortfolioProjectSql.dbo.NashvilleHousing

Select 
PARSENAME(REPLACE(OwnerAddress, ',', ',') , 3)
,PARSENAME(REPLACE(OwnerAddress, ',', ',') , 2)
,PARSENAME(REPLACE(OwnerAddress, ',', ',') , 1)
From PortfolioProjectSql.dbo.NashvilleHousing

Alter Table NashvilleHousing
Add OwnerSplitAddress Nvarchar(255);

Update NashvilleHousing
Set OwnerSplitAddress = PARSENAME(REPLACE(OwnerAddress, ',', ',') , 3)

Alter Table NashvilleHousing
Add OwnerSplitCity Nvarchar(255);

Update NashvilleHousing
Set OwnerSplitCity = PARSENAME(REPLACE(OwnerAddress, ',', ',') , 2)

Alter Table NashvilleHousing
Add OwnerSplitState Nvarchar(255);

Update NashvilleHousing
Set OwnerSplitState = PARSENAME(REPLACE(OwnerAddress, ',', ',') , 1)


Select *
From PortfolioProjectSql.dbo.NashvilleHousing

--Change Y and N to Yes and No in "Sold as Vacant" field

Select Distinct(SoldAsVacant), Count(SoldAsVacant)
From PortfolioProjectSql.dbo.NashvilleHousing
Group by SoldAsVacant
Order by 2

Select SoldAsVacant
, Case When SoldAsVacant = 'Y' THEN 'Yes'
       When SoldAsVacant = 'N' THEN 'No'
	   Else SoldAsVacant 
	   End
From PortfolioProjectSql.dbo.NashvilleHousing

Update NashvilleHousing
Set SoldAsVacant = Case When SoldAsVacant = 'Y' THEN 'Yes'
       When SoldAsVacant = 'N' THEN 'No'
	   Else SoldAsVacant 
	   End

--Remove Duplicates

With RowNumCTE As (
Select *,
     ROW_NUMBER() OVER(
	 PARTITION BY ParcelID,
	              PropertyAddress,
	              SalePrice,
	              SaleDate,
	              LegalReference
	              ORDER BY 
	                  UniqueID
		              ) row_num

From PortfolioProjectSql.dbo.NashvilleHousing
)

Select *
From RowNumCTE
Where row_num > 1
Order by PropertyAddress

--Delete 
--From RowNumCTE
--Where row_num > 1

