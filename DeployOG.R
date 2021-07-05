library(analogsea)
Sys.setenv(DO_PAT="9223d859a987c39aedef185f6be410648b62b858ddc17f40e20a3a9af359223d")

temp <- analogsea::droplets()
server <- temp$`forestvision-server`
analogsea::droplet_ssh(server,"rm -R /srv/shiny-server/phd")
analogsea::droplet_ssh(server, "mkdir /srv/shiny-server/phd")
analogsea::droplet_upload(server, "./app.R", "/srv/shiny-server/phd/app.R")
analogsea::droplet_upload(server, "./OGSource.R", "/srv/shiny-server/phd/OGSource.R")
analogsea::droplet_upload(server, "./htmlwidgets", "/srv/shiny-server/phd")
analogsea::droplet_upload(server, "./InputDat", "/srv/shiny-server/phd")
analogsea::droplet_upload(server, "./WNA_v12_HexCols.csv", "/srv/shiny-server/phd/WNA_v12_HexCols.csv")
analogsea::droplet_ssh(server, "chown -R shiny:shiny /srv/shiny-server")
analogsea::droplet_ssh(server, "systemctl restart shiny-server")
