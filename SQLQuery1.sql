Select *
From PortfolioProjectSql..CovidDeaths$
Where continent is not null
Order by 3,4

--Percentage of the population that contract Covid-19 in the United States
Select location, date, population, total_cases, (total_cases/population)*100 as percentagepopInfected
From PortfolioProjectSql..CovidDeaths$
Where continent is not null
Order by 1,2

--	Percentage likelihood of death if contract covid-19 in United States
Select location, date, total_cases, total_deaths, (total_deaths/total_cases)*100 as DeathPercentage
From PortfolioProjectSql..CovidDeaths$
Where location like '%states%'
and continent is not null
Order by 1,2

-- Countries with the highest percentage of infection compared to population
Select location, population, Max(total_cases) as highestinfectioncount, Max((total_cases/population))*100 as percentagepopInfected
From PortfolioProjectSql..CovidDeaths$
where continent is not null
Group by location, population
order by percentagepopInfected

-- Countries with the highest Covid-19 Death per population
Select location, Max(cast(total_deaths as int)) as Totaldeathcount 
From PortfolioProjectSql..CovidDeaths$
Where continent is not null
Group by location
order by Totaldeathcount desc

-- Total Death by Continent
Select continent, Max(cast(total_deaths as int)) as Totaldeathcount 
From PortfolioProjectSql..CovidDeaths$
Where continent is not null
Group by continent
order by Totaldeathcount desc

--Global Numbers
Select SUM(new_cases) as total_cases, SUM(cast(new_deaths as int)) as total_deaths, 
SUM(cast( new_deaths as int))/SUM(new_cases)*100 as Deathpercentage
From PortfolioProjectSql..CovidDeaths$
Where continent is not null 
Order by 1,2

Select *
From PortfolioProjectSql..CovidDeaths$ dea
Join PortfolioProjectSql.. CovidVaccinations$ vac
     on dea.location = vac.location
	 and dea.date = vac.date


-- Total Population and Vaccinations
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
From PortfolioProjectSql..CovidDeaths$ dea
Join PortfolioProjectSql.. CovidVaccinations$ vac
     on dea.location = vac.location
	 and dea.date = vac.date
Where dea.continent is not null
order by 2,3

-- Use CTE

With PopvsVac (Continent,location, Date, Population,New_vaccinations, Cummulativepeoplevaccinated) 
as
(
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
,SUM(CONVERT(int,vac.new_vaccinations))OVER (Partition by dea.Location Order by dea.location, dea.Date) as CummulativePeopleVaccinated
From PortfolioProjectSql..CovidDeaths$ dea
Join PortfolioProjectSql.. CovidVaccinations$ vac
     on dea.location = vac.location
	 and dea.date = vac.date
Where dea.continent is not null
)
Select *
From PopvsVac

--TEMP Table
Drop Table if exists #PercentPopulationVaccinated
Create Table #PercentPopulationVaccinated
(
Continent nvarchar(255),
Location nvarchar(255),
Date datetime,
Population numeric,
New_vaccinations numeric,
CummulativePeopleVaccinated numeric
)

Insert into #PercentPopulationVaccinated
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
,SUM(CONVERT(int,vac.new_vaccinations)) OVER (Partition by dea.Location Order by dea.location, dea.Date) as CummulativePeopleVaccinated
From PortfolioProjectSql..CovidDeaths$ dea
Join PortfolioProjectSql.. CovidVaccinations$ vac
     on dea.location = vac.location
	 and dea.date = vac.date

Select *, (CummulativePeopleVaccinated/Population)*100
From #PercentPopulationVaccinated


--Creating view to store for Visualization

Create View PercentPopulationVaccinated as
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
,SUM(CONVERT(int,vac.new_vaccinations)) OVER (Partition by dea.location Order by dea.location, dea.Date) as CummulativePeopleVaccinated
From PortfolioProjectSql..CovidDeaths$ dea
Join PortfolioProjectSql..CovidVaccinations$ vac
     on dea.location = vac.location
	 and dea.date = vac.date
Where dea.continent is not null

Select *
From PercentPopulationVaccinated