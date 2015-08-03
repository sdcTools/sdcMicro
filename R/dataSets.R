#' Census data set
#'
#' This test data set was obtained on July 27, 2000 using the public use Data
#' Extraction System of the U.S. Bureau of the Census.
#'
#'
#' @name CASCrefmicrodata
#' @docType data
#' @format A data frame sampled from year 1995 with 1080 observations on the
#' following 13 variables.  \describe{
#' \item{AFNLWGT}{Final weight (2 implied decimal places)}
#' \item{AGI}{Adjusted gross income}
#' \item{EMCONTRB}{Employer contribution for hlth insurance}
#' \item{FEDTAX}{Federal income tax liability}
#' \item{PTOTVAL}{Total person income}
#' \item{STATETAX}{State income tax liability}
#' \item{TAXINC}{Taxable income amount}
#' \item{POTHVAL}{Total other persons income}
#' \item{INTVAL}{Amt of interest income}
#' \item{PEARNVAL}{Total person earnings}
#' \item{FICA}{Soc. sec. retirement payroll deduction}
#' \item{WSALVAL}{Amount: Total Wage and salary}
#' \item{ERNVAL}{Business or Farm net earnings}}
#' @references Brand, R. and Domingo-Ferrer, J. and Mateo-Sanz, J.M., Reference
#' data sets to test and compare SDC methods for protection of numerical
#' microdata.  Unpublished.
#' \url{http://neon.vb.cbs.nl/casc/CASCrefmicrodata.pdf}
#' @source Public use file from the CASC project.  More information on this
#' test data can be found in the paper listed below.
#' @keywords datasets
#' @examples
#'
#' data(CASCrefmicrodata)
#' str(CASCrefmicrodata)
#'
NULL

#' EIA data set
#'
#' Data set obtained from the U.S. Energy Information Authority.
#'
#'
#' @name EIA
#' @docType data
#' @format A data frame with 4092 observations on the following 15 variables.
#' \describe{
#' \item{UTILITYID}{UNIQUE UTILITY IDENTIFICATION NUMBER}
#' \item{UTILNAME}{UTILITY NAME. A factor with levels \code{4-County
#' Electric Power Assn} \code{Alabama Power Co} \code{Alaska Electric}
#' \code{Appalachian Electric Coop} \code{Appalachian Power Co} \code{Arizona
#' Public Service Co} \code{Arkansas Power & Light Co} \code{Arkansas Valley
#' Elec Coop Corp} \code{Atlantic City Electric Company} \code{Baker Electric
#' Coop Inc} \code{Baltimore Gas & Electric Co} \code{Bangor Hydro-Electric Co}
#' \code{Berkeley Electric Coop Inc} \code{Black Hills Corp} \code{Blackstone
#' Valley Electric Co} \code{Bonneville Power Admin} \code{Boston Edison Co}
#' \code{Bountiful City Light & Power} \code{Bristol City of} \code{Brookings
#' City of} \code{Brunswick Electric Member Corp} \code{Burlington City of}
#' \code{Carolina Power & Light Co} \code{Carroll Electric Coop Corp}
#' \code{Cass County Electric Coop Inc} \code{Central Illinois Light Company}
#' \code{Central Illinois Pub Serv Co} \code{Central Louisiana Elec Co Inc}
#' \code{Central Maine Power Co} \code{Central Power & Light Co} \code{Central
#' Vermont Pub Serv Corp} \code{Chattanooga City of} \code{Cheyenne Light Fuel
#' & Power Co} \code{Chugach Electric Assn Inc} \code{Cincinnati Gas & Electric
#' Co} \code{Citizens Utilities Company} \code{City of Boulder City} \code{City
#' of Clinton} \code{City of Dover} \code{City of Eugene} \code{City of
#' Gillette} \code{City of Groton Dept of Utils} \code{City of Idaho Falls}
#' \code{City of Independence} \code{City of Newark} \code{City of Reading}
#' \code{City of Tupelo Water & Light D} \code{Clarksville City of}
#' \code{Cleveland City of} \code{Cleveland Electric Illum Co} \code{Coast
#' Electric Power Assn} \code{Cobb Electric Membership Corp} \code{Colorado
#' River Commission} \code{Colorado Springs City of} \code{Columbus Southern
#' Power Co} \code{Commonwealth Edison Co} \code{Commonwealth Electric Co}
#' \code{Connecticut Light & Power Co} \code{Consolidated Edison Co-NY Inc}
#' \code{Consumers Power Co} \code{Cornhusker Public Power Dist} \code{Cuivre
#' River Electric Coop Inc} \code{Cumberland Elec Member Corp} \code{Dakota
#' Electric Assn} \code{Dawson County Public Pwr Dist} \code{Dayton Power &
#' Light Company} \code{Decatur City of} \code{Delaware Electric Coop Inc}
#' \code{Delmarva Power & Light Co} \code{Detroit Edison Co} \code{Duck River
#' Elec Member Corp} \code{Duke Power Co} \code{Duquesne Light Company}
#' \code{East Central Electric Assn} \code{Eastern Maine Electric Coop}
#' \code{El Paso Electric Co} \code{Electric Energy Inc} \code{Empire District
#' Electric Co} \code{Exeter & Hampton Electric Co} \code{Fairbanks City of}
#' \code{Fayetteville Public Works Comm} \code{First Electric Coop Corp}
#' \code{Florence City of} \code{Florida Power & Light Co} \code{Florida Power
#' Corp} \code{Fort Collins Lgt & Pwr Utility} \code{Fremont City of}
#' \code{Georgia Power Co} \code{Gibson County Elec Member Corp} \code{Golden
#' Valley Elec Assn Inc} \code{Grand Island City of} \code{Granite State
#' Electric Co} \code{Green Mountain Power Corp} \code{Green River Electric
#' Corp} \code{Greeneville City of} \code{Gulf Power Company} \code{Gulf States
#' Utilities Co} \code{Hasting Utilities} \code{Hawaii Electric Light Co Inc}
#' \code{Hawaiian Electric Co Inc} \code{Henderson-Union Rural E C C}
#' \code{Homer Electric Assn Inc} \code{Hot Springs Rural El Assn Inc}
#' \code{Houston Lighting & Power Co} \code{Huntsville City of} \code{Idaho
#' Power Co} \code{IES Utilities Inc} \code{Illinois Power Co} \code{Indiana
#' Michigan Power Co} \code{Indianapolis Power & Light Co} \code{Intermountain
#' Rural Elec Assn} \code{Interstate Power Co} \code{Jackson Electric Member
#' Corp} \code{Jersey Central Power&Light Co} \code{Joe Wheeler Elec Member
#' Corp} \code{Johnson City City of} \code{Jones-Onslow Elec Member Corp}
#' \code{Kansas City City of} \code{Kansas City Power & Light Co}
#' \code{Kentucky Power Co} \code{Kentucky Utilities Co} \code{Ketchikan Public
#' Utilities} \code{Kingsport Power Co} \code{Knoxville City of} \code{Kodiak
#' Electric Assn Inc} \code{Kootenai Electric Coop, Inc} \code{Lansing Board of
#' Water & Light} \code{Lenoir City City of} \code{Lincoln City of} \code{Long
#' Island Lighting Co} \code{Los Angeles City of} \code{Louisiana Power & Light
#' Co} \code{Louisville Gas & Electric Co} \code{Loup River Public Power Dist}
#' \code{Lower Valley Power & Light Inc} \code{Maine Public Service Company}
#' \code{Massachusetts Electric Co} \code{Matanuska Electric Assn Inc}
#' \code{Maui Electric Co Ltd} \code{McKenzie Electric Coop Inc} \code{Memphis
#' City of} \code{MidAmerican Energy Company} \code{Middle Tennessee E M C}
#' \code{Midwest Energy, Inc} \code{Minnesota Power & Light Co}
#' \code{Mississippi Power & Light Co} \code{Mississippi Power Co}
#' \code{Monongahela Power Co} \code{Montana-Dakota Utilities Co} \code{Montana
#' Power Co} \code{Moon Lake Electric Assn Inc} \code{Narragansett Electric Co}
#' \code{Nashville City of} \code{Nebraska Public Power District} \code{Nevada
#' Power Co} \code{New Hampshire Elec Coop, Inc} \code{New Orleans Public
#' Service Inc} \code{New York State Gas & Electric} \code{Newport Electric
#' Corp} \code{Niagara Mohawk Power Corp} \code{Nodak Rural Electric Coop Inc}
#' \code{Norris Public Power District} \code{Northeast Oklahoma Electric Co}
#' \code{Northern Indiana Pub Serv Co} \code{Northern States Power Co}
#' \code{Northwestern Public Service Co} \code{Ohio Edison Co} \code{Ohio Power
#' Co} \code{Ohio Valley Electric Corp} \code{Oklahoma Electric Coop, Inc}
#' \code{Oklahoma Gas & Electric Co} \code{Oliver-Mercer Elec Coop, Inc}
#' \code{Omaha Public Power District} \code{Otter Tail Power Co} \code{Pacific
#' Gas & Electric Co} \code{Pacificorp dba Pacific Pwr & L} \code{Palmetto
#' Electric Coop, Inc} \code{Pennsylvania Power & Light Co} \code{Pennyrile
#' Rural Electric Coop} \code{Philadelphia Electric Co} \code{Pierre Municipal
#' Electric} \code{Portland General Electric Co} \code{Potomac Edison Co}
#' \code{Potomac Electric Power Co} \code{Poudre Valley R E A, Inc} \code{Power
#' Authority of State of NY} \code{Provo City Corporation} \code{Public Service
#' Co of Colorado} \code{Public Service Co of IN Inc} \code{Public Service Co
#' of NH} \code{Public Service Co of NM} \code{Public Service Co of Oklahoma}
#' \code{Public Service Electric&Gas Co} \code{PUD No 1 of Clark County}
#' \code{PUD No 1 of Snohomish County} \code{Puget Sound Power & Light Co}
#' \code{Rappahannock Electric Coop} \code{Rochester Public Utilities}
#' \code{Rockland Electric Company} \code{Rosebud Electric Coop Inc}
#' \code{Rutherford Elec Member Corp} \code{Sacramento Municipal Util Dist}
#' \code{Salmon River Electric Coop Inc} \code{Salt River Proj Ag I & P Dist}
#' \code{San Antonio City of} \code{Savannah Electric & Power Co} \code{Seattle
#' City of} \code{Sierra Pacific Power Co} \code{Singing River Elec Power Assn}
#' \code{Sioux Valley Empire E A Inc} \code{South Carolina Electric&Gas Co}
#' \code{South Carolina Pub Serv Auth} \code{South Kentucky Rural E C C}
#' \code{Southern California Edison Co} \code{Southern Nebraska Rural P P D}
#' \code{Southern Pine Elec Power Assn} \code{Southwest Tennessee E M C}
#' \code{Southwestern Electric Power Co} \code{Southwestern Public Service Co}
#' \code{Springfield City of} \code{St Joseph Light & Power Co} \code{State
#' Level Adjustment} \code{Tacoma City of} \code{Tampa Electric Co}
#' \code{Texas-New Mexico Power Co} \code{Texas Utilities Electric Co}
#' \code{Tri-County Electric Assn Inc} \code{Tucson Electric Power Co}
#' \code{Turner-Hutchinsin El Coop, Inc} \code{TVA} \code{U S Bureau of Indian
#' Affairs} \code{Union Electric Co} \code{Union Light Heat & Power Co}
#' \code{United Illuminating Co} \code{Upper Cumberland E M C} \code{UtiliCorp
#' United Inc} \code{Verdigris Valley Electric Coop} \code{Verendrye Electric
#' Coop Inc} \code{Virginia Electric & Power Co} \code{Volunteer Electric Coop}
#' \code{Wallingford Town of} \code{Warren Rural Elec Coop Corp}
#' \code{Washington Water Power Co} \code{Watertown Municipal Utils Dept}
#' \code{Wells Rural Electric Co} \code{West Penn Power Co} \code{West Plains
#' Electric Coop Inc} \code{West River Electric Assn, Inc} \code{Western
#' Massachusetts Elec Co} \code{Western Resources Inc} \code{Wheeling Power
#' Company} \code{Wisconsin Electric Power Co} \code{Wisconsin Power & Light
#' Co} \code{Wisconsin Public Service Corp} \code{Wright-Hennepin Coop Elec
#' Assn} \code{Yellowstone Vlly Elec Coop Inc}}
#' \item{STATE}{STATE FOR WHICH THE UTILITY IS REPORTING. A factor with levels
#' \code{AK} \code{AL} \code{AR} \code{AZ} \code{CA} \code{CO} \code{CT}
#' \code{DC} \code{DE} \code{FL} \code{GA} \code{HI} \code{IA} \code{ID}
#' \code{IL} \code{IN} \code{KS} \code{KY} \code{LA} \code{MA} \code{MD}
#' \code{ME} \code{MI} \code{MN} \code{MO} \code{MS} \code{MT} \code{NC}
#' \code{ND} \code{NE} \code{NH} \code{NJ} \code{NM} \code{NV} \code{NY}
#' \code{OH} \code{OK} \code{OR} \code{PA} \code{RI} \code{SC} \code{SD}
#' \code{TN} \code{TX} \code{UT} \code{VA} \code{VT} \code{WA} \code{WI}
#' \code{WV} \code{WY}}
#' \item{YEAR}{REPORTING YEAR FOR THE DATA}
#' \item{MONTH}{REPORTING MONTH FOR THE DATA}
#' \item{RESREVENUE}{REVENUE FROM SALES TO RESIDENTIAL CONSUMERS}
#' \item{RESSALES}{SALES TO RESIDENTIAL CONSUMERS}
#' \item{COMREVENUE}{REVENUE FROM SALES TO COMMERCIAL CONSUMERS}
#' \item{COMSALES}{SALES TO COMMERCIAL CONSUMERS}
#' \item{INDREVENUE}{REVENUE FROM SALES TO INDUSTRIAL CONSUMERS}
#' \item{INDSALES}{SALES TO INDUSTRIAL CONSUMERS}
#' \item{OTHREVENUE}{REVENUE FROM SALES TO OTHER CONSUMERS}
#' \item{OTHRSALES}{SALES TO OTHER CONSUMERS}
#' \item{TOTREVENUE}{REVENUE FROM SALES TO ALL CONSUMERS}
#' \item{TOTSALES}{SALES TO ALL CONSUMERS}}
#' @references Brand, R. and Domingo-Ferrer, J. and Mateo-Sanz, J.M., Reference
#' data sets to test and compare SDC methods for protection of numerical
#' microdata. Unpublished.
#' \url{http://neon.vb.cbs.nl/casc/CASCrefmicrodata.pdf}
#' @source Public use file from the CASC project.
#' @keywords datasets
#' @examples
#'
#' data(EIA)
#' head(EIA)
#'
NULL


#' Tarragona data set
#'
#' A real data set comprising figures of 834 companies in the Tarragona area.
#' Data correspond to year 1995.
#'
#'
#' @name Tarragona
#' @docType data
#' @format A data frame with 834 observations on the following 13 variables.
#' \describe{
#' \item{FIXED.ASSETS}{a numeric vector}
#' \item{CURRENT.ASSETS}{a numeric vector}
#' \item{TREASURY}{a numeric vector}
#' \item{UNCOMMITTED.FUNDS}{a numeric vector}
#' \item{PAID.UP.CAPITAL}{a numeric vector}
#' \item{SHORT.TERM.DEBT}{a numeric vector}
#' \item{SALES}{a numeric vector}
#' \item{LABOR.COSTS}{a numeric vector}
#' \item{DEPRECIATION}{a numeric vector}
#' \item{OPERATING.PROFIT}{a numeric vector}
#' \item{FINANCIAL.OUTCOME}{a numeric vector}
#' \item{GROSS.PROFIT}{a numeric vector}
#' \item{NET.PROFIT}{a numeric vector}}
#' @references Brand, R. and Domingo-Ferrer, J. and Mateo-Sanz, J.M., Reference
#' data sets to test and compare SDC methods for protection of numerical
#' microdata. Unpublished.
#' \url{http://neon.vb.cbs.nl/casc/CASCrefmicrodata.pdf}
#' @source Public use data from the CASC project.
#' @keywords datasets
#' @examples
#'
#' data(Tarragona)
#' head(Tarragona)
#' dim(Tarragona)
NULL


#' Small Artificial Data set
#'
#' Small Toy Example Data set which was used by Sanz-Mateo et.al.
#'
#' @name casc1
#' @docType data
#' @format The format is: int [1:13, 1:7] 10 12 17 21 9 12 12 14 13 15 ...  -
#' attr(*, "dimnames")=List of 2 ..$ : chr [1:13] "1" "2" "3" "4" ...  ..$ :
#' chr [1:7] "1" "2" "3" "4" ...
#' @keywords datasets
#' @examples
#'
#' data(casc1)
#' casc1
#'
NULL


#' data from the casc project
#'
#' Small synthetic data from Capobianchi, Polettini, Lucarelli
#'
#' This data set is very similar to that one which are used by the authors of
#' the paper given below. We need this data set only for demonstration effect,
#' i.e. that the package provides the same results as their software.
#'
#' @name francdat
#' @docType data
#' @format A data frame with 8 observations on the following 8 variables.
#' \describe{
#' \item{Num1}{a numeric vector}
#' \item{Key1}{Key variable 1. A numeric vector}
#' \item{Num2}{a numeric vector}
#' \item{Key2}{Key variable 2. A numeric vector}
#' \item{Key3}{Key variable 3. A numeric vector}
#' \item{Key4}{Key variable 4. A numeric vector}
#' \item{Num3}{a numeric vector}
#' \item{w}{The weight vector. A numeric vector}}
#' @source \url{http://neon.vb.cbs.nl/casc/Deliv/12d1.pdf}
#' @keywords datasets
#' @examples
#'
#' data(francdat)
#' francdat
NULL


#' Demo data set from mu-Argus
#'
#' The public use toy demo data set from the mu-Argus software for SDC.
#'
#' Please, see at the link given below. Please note, that the correlation
#' structure of the data is not very realistic, especially concerning the
#' continuous scaled variables which drawn independently from are a
#' multivariate uniform distribution.
#'
#' @name free1
#' @docType data
#' @format The format is: num [1:4000, 1:34] 36 36 36 36 36 36 36 36 36 36 ...
#' - attr(*, "dimnames")=List of 2 ..$ : NULL ..$ : chr [1:34] "REGION" "SEX"
#' "AGE" "MARSTAT" ...
#' @source Public use file from the CASC project.
#' @keywords datasets
#' @examples
#'
#' data(free1)
#' head(free1)
#'
NULL


#' microData
#'
#' Small aritificial toy data set.
#'
#'
#' @name microData
#' @docType data
#' @format The format is: num [1:13, 1:5] 5 7 2 1 7 8 12 3 15 4 ...  - attr(*,
#' "dimnames")=List of 2 ..$ : chr [1:13] "10000" "11000" "12000" "12100" ...
#' ..$ : chr [1:5] "one" "two" "three" "four" ...
#' @keywords datasets
#' @examples
#'
#' data(microData)
#' m1 <- microaggregation(microData, method="mdav")
#' summary(m1)
#'
NULL


#' A real-world data set on household income and expenditures
#'
#' A concise (1-5 lines) description of the dataset.
#'
#'
#' @name testdata
#' @aliases testdata testdata2
#' @docType data
#' @format A data frame with 4580 observations on the following 14 variables.
#' \describe{
#' \item{urbrur}{a numeric vector}
#' \item{roof}{a numeric vector}
#' \item{walls}{a numeric vector}
#' \item{water}{a numeric vector}
#' \item{electcon}{a numeric vector}
#' \item{relat}{a numeric vector}
#' \item{sex}{a numeric vector}
#' \item{age}{a numeric vector}
#' \item{hhcivil}{a numeric vector}
#' \item{expend}{a numeric vector}
#' \item{income}{a numeric vector}
#' \item{savings}{a numeric vector}
#' \item{ori_hid}{a numeric vector}
#' \item{sampling_weight}{a numeric vector}}
#' A data frame with 93 observations on the following 19 variables.
#' \describe{
#' \item{urbrur}{a numeric vector}
#' \item{roof}{a numeric vector}
#' \item{walls}{a numeric vector}
#' \item{water}{a numeric vector}
#' \item{electcon}{a numeric vector}
#' \item{relat}{a numeric vector}
#' \item{sex}{a numeric vector}
#' \item{age}{a numeric vector}
#' \item{hhcivil}{a numeric vector}
#' \item{expend}{a numeric vector}
#' \item{income}{a numeric vector}
#' \item{savings}{a numeric vector}
#' \item{ori_hid}{a numeric vector}
#' \item{sampling_weight}{a numeric vector}
#' \item{represent}{a numeric vector}
#' \item{category_count}{a numeric vector}
#' \item{relat2}{a numeric vector}
#' \item{water2}{a numeric vector}
#' \item{water3}{a numeric vector}}
#' @references The International Household Survey Network, www.ihsn.org
#' @keywords datasets
#' @examples
#'
#' data(testdata)
#' ## maybe str(testdata) ; plot(testdata) ...
#'
NULL
