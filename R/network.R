# Hello, MATSim world!
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
# -----------------------------------------------------------

#' @import dplyr
#' @import readr
#' @import purrr
#' @import xml2
#' @import sf
#' @import ggalluvial
#' @import ggrepel
#' @import tidyverse
#' @import tidyr
#' @import ggplot2
#' @import yaml
#' @import plotly
#' @import leaflet
#' @import forcats
#' @import lubridate
#' @import htmltools
#' @import reshape

# NETWORK = "kelheim.output_network.xml"
# NETWORK = "network.xml"
NETWORK <- "berlin.network.xml.gz"

# prevent warning about global use of "name". Thanks dplyr
utils::globalVariables(c("name"))

# ------

#' Load MATSim network into memory
#'
#' Loads a MATSim XML network file, creating a nodes tibble and a links tibble.
#' Any node and link attribute records in the network are stored as
#' additional columns in the respective node and link tibbles.
#'
#' The links table is automatically joined with the nodes table so that
#' node x/y coordinates (and any other node attributes) are available on the
#' links table without additional processing.
#'
#' @param filename File to load. Can be XML or gzipped XML
#'
#' @return "nodes" and "links" tibbles in a list object.
#'
#' @export
read_network <- function(filename) {
  cat(filename, ": ")
  network <- read_xml(filename)

  cat("Nodes..")
  node_elements <- network %>% xml_find_all("./nodes/node")
  nodes <- tibble(
    id = node_elements %>% xml_attr("id"),
    x = node_elements %>% xml_attr("x") %>% parse_double(),
    y = node_elements %>% xml_attr("y") %>% parse_double(),
  )

  attributes <- node_elements %>% xml_find_all("./attributes/attribute")
  # skip if there are no node attributes
  if (length(attributes)) {
    attrIds <- tibble(id = unlist(map(attributes, function(row) xml_attr(xml_parent(xml_parent(row)), "id"))))

    nodeAttributes <- tibble(
      id = attrIds$id,
      name = attributes %>% xml_attr("name"),
      class = attributes %>% xml_attr("class"),
      value = attributes %>% xml_text(),
    )

    # which columns should be converted to numeric?
    types <- nodeAttributes %>%
      select(name, class) %>%
      distinct()
    convert <- filter(types, class == "java.lang.Double")$name

    # convert to a format we can join to the links
    nodeAttributes <- (nodeAttributes
    %>% select(-class)
      %>% pivot_wider(names_from = "name", values_from = "value")
      %>% mutate_at(vars(one_of(convert)), as.double)
    )
    nodes <- nodes %>% left_join(nodeAttributes, by = "id")
  }

  cat("Links..")
  link_elements <- network %>% xml_find_all("./links/link")
  links <- tibble(
    id = link_elements %>% xml_attr("id"),
    from = link_elements %>% xml_attr("from"),
    to = link_elements %>% xml_attr("to"),
    length = link_elements %>% xml_attr("length") %>% parse_double(),
    freespeed = link_elements %>% xml_attr("freespeed") %>% parse_double(),
    capacity = link_elements %>% xml_attr("capacity") %>% parse_double(),
    permlanes = link_elements %>% xml_attr("permlanes") %>% parse_double(),
    modes = link_elements %>% xml_attr("modes"),
    origid = link_elements %>% xml_attr("origid")
  )

  # merge node coordinates
  links <- (links
  %>% left_join(nodes, by = c("from" = "id"))
    %>% left_join(nodes, by = c("to" = "id"), suffix = c(".from", ".to"))
  )

  # attributes don't have IDs on them! JFC, MATSim!
  cat("Attributes..")

  attributes <- link_elements %>% xml_find_all("./attributes/attribute")
  # skip if there are no link-attributes
  if (length(attributes)) {
    attrIds <- tibble(id = unlist(map(attributes, function(row) xml_attr(xml_parent(xml_parent(row)), "id"))))

    linkAttributes <- tibble(
      id = attrIds$id,
      name = attributes %>% xml_attr("name"),
      class = attributes %>% xml_attr("class"),
      value = attributes %>% xml_text(),
    )

    # which columns should be converted to numeric?
    types <- linkAttributes %>%
      select(name, class) %>%
      distinct()
    convert <- filter(types, class == "java.lang.Double")$name

    # convert to a format we can join to the links
    linkAttributes <- (linkAttributes
    %>% select(-class)
      %>% pivot_wider(names_from = "name", values_from = "value")
      %>% mutate_at(vars(one_of(convert)), as.double)
    )

    links <- links %>% left_join(linkAttributes, by = "id", suffix = c(".link", ".attr"))
  }

  # Top-level network attributes
  networkAttributes <- NULL

  allNetworkAttributes <- network %>% xml_find_all("./attributes/attribute")
  if (length(allNetworkAttributes)) {
    networkAttributes <- tibble(
      name = allNetworkAttributes %>% xml_attr("name"),
      class = allNetworkAttributes %>% xml_attr("class"),
      value = allNetworkAttributes %>% xml_text(),
    )
    # which columns should be converted to numeric?
    types <- networkAttributes %>%
      select(name, class) %>%
      distinct()
    convert <- filter(types, class == "java.lang.Double")$name

    # convert to a format we can join to the links
    networkAttributes <- (networkAttributes
    %>% select(-class)
      %>% pivot_wider(names_from = "name", values_from = "value")
      %>% mutate_at(vars(one_of(convert)), as.double)
    )
  }

  cat("Done!\n")

  list("nodes" = nodes, "links" = links, "attributes" = networkAttributes)
}


#' Load MATSim network into memory
#'
#' Loads a MATSim XML network file, creating a nodes tibble and a links tibble.
#' Any node and link attribute records in the network are stored as
#' additional columns in the respective node and link tibbles.
#'
#' The links table is automatically joined with the nodes table so that
#' node x/y coordinates (and any other node attributes) are available on the
#' links table without additional processing.
#'
#' @param filename File to load. Can be XML or gzipped XML
#'
#' @return "nodes" and "links" tibbles in a list object.
#'
#' @export
loadNetwork <- function(filename) {
  .Deprecated("read_network")
  cat(filename, ": ")
  network <- read_xml(filename)

  cat("Nodes..")
  node_elements <- network %>% xml_find_all("./nodes/node")
  nodes <- tibble(
    id = node_elements %>% xml_attr("id"),
    x = node_elements %>% xml_attr("x") %>% parse_double(),
    y = node_elements %>% xml_attr("y") %>% parse_double(),
  )

  attributes <- node_elements %>% xml_find_all("./attributes/attribute")
  # skip if there are no node attributes
  if (length(attributes)) {
    attrIds <- tibble(id = unlist(map(attributes, function(row) xml_attr(xml_parent(xml_parent(row)), "id"))))

    nodeAttributes <- tibble(
      id = attrIds$id,
      name = attributes %>% xml_attr("name"),
      class = attributes %>% xml_attr("class"),
      value = attributes %>% xml_text(),
    )

    # which columns should be converted to numeric?
    types <- nodeAttributes %>%
      select(name, class) %>%
      distinct()
    convert <- filter(types, class == "java.lang.Double")$name

    # convert to a format we can join to the links
    nodeAttributes <- (nodeAttributes
                       %>% select(-class)
                       %>% pivot_wider(names_from = "name", values_from = "value")
                       %>% mutate_at(vars(one_of(convert)), as.double)
    )
    nodes <- nodes %>% left_join(nodeAttributes, by = "id")
  }

  cat("Links..")
  link_elements <- network %>% xml_find_all("./links/link")
  links <- tibble(
    id = link_elements %>% xml_attr("id"),
    from = link_elements %>% xml_attr("from"),
    to = link_elements %>% xml_attr("to"),
    length = link_elements %>% xml_attr("length") %>% parse_double(),
    freespeed = link_elements %>% xml_attr("freespeed") %>% parse_double(),
    capacity = link_elements %>% xml_attr("capacity") %>% parse_double(),
    permlanes = link_elements %>% xml_attr("permlanes") %>% parse_double(),
    modes = link_elements %>% xml_attr("modes"),
    origid = link_elements %>% xml_attr("origid")
  )

  # merge node coordinates
  links <- (links
            %>% left_join(nodes, by = c("from" = "id"))
            %>% left_join(nodes, by = c("to" = "id"), suffix = c(".from", ".to"))
  )

  # attributes don't have IDs on them! JFC, MATSim!
  cat("Attributes..")

  attributes <- link_elements %>% xml_find_all("./attributes/attribute")
  # skip if there are no link-attributes
  if (length(attributes)) {
    attrIds <- tibble(id = unlist(map(attributes, function(row) xml_attr(xml_parent(xml_parent(row)), "id"))))

    linkAttributes <- tibble(
      id = attrIds$id,
      name = attributes %>% xml_attr("name"),
      class = attributes %>% xml_attr("class"),
      value = attributes %>% xml_text(),
    )

    # which columns should be converted to numeric?
    types <- linkAttributes %>%
      select(name, class) %>%
      distinct()
    convert <- filter(types, class == "java.lang.Double")$name

    # convert to a format we can join to the links
    linkAttributes <- (linkAttributes
                       %>% select(-class)
                       %>% pivot_wider(names_from = "name", values_from = "value")
                       %>% mutate_at(vars(one_of(convert)), as.double)
    )

    links <- links %>% left_join(linkAttributes, by = "id", suffix = c(".link", ".attr"))
  }

  # Top-level network attributes
  networkAttributes <- NULL

  allNetworkAttributes <- network %>% xml_find_all("./attributes/attribute")
  if (length(allNetworkAttributes)) {
    networkAttributes <- tibble(
      name = allNetworkAttributes %>% xml_attr("name"),
      class = allNetworkAttributes %>% xml_attr("class"),
      value = allNetworkAttributes %>% xml_text(),
    )
    # which columns should be converted to numeric?
    types <- networkAttributes %>%
      select(name, class) %>%
      distinct()
    convert <- filter(types, class == "java.lang.Double")$name

    # convert to a format we can join to the links
    networkAttributes <- (networkAttributes
                          %>% select(-class)
                          %>% pivot_wider(names_from = "name", values_from = "value")
                          %>% mutate_at(vars(one_of(convert)), as.double)
    )
  }

  cat("Done!\n")

  list("nodes" = nodes, "links" = links, "attributes" = networkAttributes)
}
