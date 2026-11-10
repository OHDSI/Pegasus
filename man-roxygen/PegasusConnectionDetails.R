#' @param pegasusConnection        A connection object created using `DatabaseConnector::connect()` for connecting to
#'                                 the database holding the Pegasus data model. Can be null if
#'                                 `pegasusConnectionDetails` is provided.
#' @param pegasusConnectionDetails An R object of type `connectionDetails` created using the
#'                                 [DatabaseConnector::createConnectionDetails()] function for connecting to the
#'                                 database holding the Pegasus data model. Can be null if
#'                                 `pegasusConnection` is provided.
#' @param pegasusDatabaseSchema    The name of the database schema holding the Pegasus data model
