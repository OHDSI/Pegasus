# Copyright 2026 Observational Health Data Sciences and Informatics
#
# This file is part of Pegasus
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and

# Routing for the three landing pages (phenotype, cohort definition, concept set). A page is addressed by
# a query string, for example ?page=cohort_definition&id=12, so every page has a static, shareable URL. The
# route is the single source of truth: links and the browser's back and forward buttons change it, and the
# address bar follows (see server.R).

routePages <- c("phenotype", "cohort_definition", "concept_set")

# A valid route is a known page with a positive whole-number ID; anything else is not a route.
makeRoute <- function(page, id) {
  if (length(page) != 1 || is.na(page) || !page %in% routePages) {
    return(NULL)
  }
  idText <- if (is.numeric(id)) format(id, scientific = FALSE, trim = TRUE) else as.character(id)
  if (length(idText) != 1 || is.na(idText) || !grepl("^[0-9]{1,9}$", idText) || as.integer(idText) < 1) {
    return(NULL)
  }
  list(page = page, id = as.integer(idText))
}

# Reads a route from a URL query string such as "?page=phenotype&id=5". NULL means the Browse list.
parseRoute <- function(search) {
  if (is.null(search) || length(search) != 1 || is.na(search) || !nzchar(search)) {
    return(NULL)
  }
  query <- shiny::parseQueryString(search)
  makeRoute(query$page, query$id)
}

# The query string for a route; "?" (no parameters) for the list.
routeQuery <- function(route) {
  if (is.null(route)) "?" else sprintf("?page=%s&id=%d", route$page, route$id)
}

# A link to a landing page for use in HTML strings (table cells). The href is a real URL, so the link can be
# copied or opened in a new tab; an ordinary click is handled by the script below without reloading the page.
entityLink <- function(page, id, label) {
  link <- rep("", length(id))
  valid <- !is.na(id) & page %in% routePages
  link[valid] <- sprintf(
    '<a href="%s" class="entity-link" data-page="%s" data-id="%d">%s</a>',
    htmltools::htmlEscape(vapply(id[valid], function(x) routeQuery(makeRoute(page, x)), ""), attribute = TRUE),
    page,
    as.integer(id[valid]),
    htmltools::htmlEscape(label[valid])
  )
  link
}

# Several links in one line, for example the parents of a phenotype.
entityLinkList <- function(page, ids, labels) {
  if (length(ids) == 0) {
    return(htmltools::HTML("none"))
  }
  htmltools::HTML(paste(entityLink(page, ids, labels), collapse = ", "))
}

# A button that copies the page's address to the clipboard. It uses the current address of the app, so it
# works behind any proxy or content URL.
copyLinkButton <- function(route) {
  htmltools::tags$button(
    type = "button",
    class = "btn btn-outline-secondary btn-sm",
    onclick = sprintf("pegasusCopyLink('%s', %d, this)", route$page, route$id),
    shiny::icon("link"),
    " Copy link"
  )
}

# Builds the navigate(page, id, push) function used throughout the app. It changes the route (page NULL is the
# Browse list), adds a browser history entry through updateUrl when push is TRUE and the route actually
# changed, and always brings the Browse tab to the front through showBrowse. An invalid page or ID is ignored.
createNavigator <- function(route, updateUrl, showBrowse) {
  function(page = NULL, id = NULL, push = TRUE) {
    target <- if (is.null(page)) NULL else makeRoute(page, id)
    if (!is.null(page) && is.null(target)) {
      return(invisible(NULL))
    }
    if (!identical(target, route())) {
      route(target)
      if (push) {
        updateUrl(routeQuery(target))
      }
    }
    showBrowse()
    invisible(target)
  }
}

# A button that copies the text of the element with the given ID (for example a block of JSON).
copyTextButton <- function(targetId, label = "Copy to clipboard") {
  htmltools::tags$button(
    type = "button",
    class = "btn btn-outline-secondary btn-sm",
    onclick = sprintf("pegasusCopyText('%s', this)", targetId),
    shiny::icon("copy"),
    paste0(" ", label)
  )
}

routingScript <- "
document.addEventListener('click', function(event) {
  var link = event.target.closest ? event.target.closest('a.entity-link') : null;
  if (!link || event.button !== 0 || event.ctrlKey || event.metaKey || event.shiftKey || event.altKey) { return; }
  event.preventDefault();
  Shiny.setInputValue('navigateTo', {page: link.dataset.page, id: parseInt(link.dataset.id, 10)}, {priority: 'event'});
});
window.addEventListener('popstate', function() {
  Shiny.setInputValue('routeSearch', window.location.search, {priority: 'event'});
});
function pegasusCopy(text, button, promptLabel) {
  var done = function() {
    var original = button.innerHTML;
    button.textContent = 'Copied';
    setTimeout(function() { button.innerHTML = original; }, 1500);
  };
  if (navigator.clipboard && window.isSecureContext) {
    navigator.clipboard.writeText(text).then(done, function() { window.prompt(promptLabel, text); });
  } else {
    window.prompt(promptLabel, text);
  }
}
function pegasusCopyLink(page, id, button) {
  pegasusCopy(window.location.origin + window.location.pathname + '?page=' + page + '&id=' + id, button, 'Copy this link:');
}
function pegasusCopyText(elementId, button) {
  var element = document.getElementById(elementId);
  if (element) { pegasusCopy(element.textContent, button, 'Copy this text:'); }
}"

routingHead <- function() {
  htmltools::tags$script(htmltools::HTML(routingScript))
}
