# ces - national, state, msa
# esapplic -state, county, planning region, workforce investment region, MSA, VA community college region
# industry - state, county, planning region, workforce investment region, congressional district, MSA, VA community college region
# industry_occupation_projections (iomatrix) - state, workforce investment region
# labforce - state, county, planning region, workforce investment region, MSA, VA community college region
# largest_employers - state, county, planning region, workforce investment region, congressional district, MSA, VA community college region
# startup_firms - state, county, planning region, workforce investment region, MSA, VA community college region
# uiclaims_continued - state, county, planning region, workforce investment region, MSA, VA community college region
# uiclaims_initials - state, county, planning region, workforce investment region, MSA, VA Community college region, ZIP CODE





download.file(
  url = "https://virginiaworks.com/Portals/200/Download Center/uiclaims_initials/uiclaims_initials.zip?ver=2020-06-15-081203-340",
  destfile = "data/vworks/uiclaims_initials.zip")

# unzip wouldn't work so I downloaded and uploaded the file

#unzip("/sfs/qumulo/qhome/sm9dv/dspg20fairfax/data/vworks/uiclaims_initials.zip")

#system(command = "unzip -c data/vworks/uiclaims_initials.zip")









