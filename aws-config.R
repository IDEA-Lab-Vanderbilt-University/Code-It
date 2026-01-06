# aws-config.R
library(jsonlite)
library(httr)

# AWS Cognito configuration
get_cognito_config <- function() {
  list(
    user_pool_id = Sys.getenv("COGNITO_USER_POOL_ID"),
    client_id = Sys.getenv("COGNITO_CLIENT_ID"),
    region = Sys.getenv("AWS_REGION")
  )
}

# Authenticate user with Cognito
authenticate_cognito_user <- function(username, password) {
  config <- get_cognito_config()
  auth_url <- paste0("https://cognito-idp.", config$region, ".amazonaws.com/")

  body <- list(
    AuthFlow = "USER_PASSWORD_AUTH",
    ClientId = config$client_id,
    AuthParameters = list(
      USERNAME = username,
      PASSWORD = password
    )
  )

  response <- POST(
    url = auth_url,
    add_headers(
      "X-Amz-Target" = "AWSCognitoIdentityProviderService.InitiateAuth",
      "Content-Type" = "application/x-amz-json-1.1"
    ),
    body = toJSON(body, auto_unbox = TRUE)
  )

  if (status_code(response) == 200) {
    response_text <- httr::content(response, "text", encoding = "UTF-8")
    result <- fromJSON(response_text)

    if (!is.null(result$AuthenticationResult)) {
      return(list(success = TRUE, user = username, tokens = result$AuthenticationResult))
    }
  }

  # Parse error message
  if (status_code(response) == 400) {
    response_text <- httr::content(response, "text", encoding = "UTF-8")
    error_info <- fromJSON(response_text)
    return(list(success = FALSE, message = error_info$message))
  }

  return(list(success = FALSE, message = "Authentication failed"))
}

# Register new user with Cognito
register_cognito_user <- function(username, password, email) {
  config <- get_cognito_config()
  auth_url <- paste0("https://cognito-idp.", config$region, ".amazonaws.com/")

  body <- list(
    ClientId = config$client_id,
    Username = username,
    Password = password,
    UserAttributes = list(
      list(Name = "email", Value = email)
    )
  )

  response <- POST(
    url = auth_url,
    add_headers(
      "X-Amz-Target" = "AWSCognitoIdentityProviderService.SignUp",
      "Content-Type" = "application/x-amz-json-1.1"
    ),
    body = toJSON(body, auto_unbox = TRUE)
  )

  if (status_code(response) == 200) {
    response_text <- httr::content(response, "text", encoding = "UTF-8")
    result <- fromJSON(response_text)
    return(list(success = TRUE, message = "Registration successful! Please check your email to verify your account.", user_sub = result$UserSub))
  } else {
    response_content <- httr::content(response, "text", encoding = "UTF-8")
    error_info <- fromJSON(response_content)
    return(list(success = FALSE, message = error_info$message))
  }
}

# Confirm user registration
confirm_cognito_user <- function(username, confirmation_code) {
  config <- get_cognito_config()
  auth_url <- paste0("https://cognito-idp.", config$region, ".amazonaws.com/")

  body <- list(
    ClientId = config$client_id,
    Username = username,
    ConfirmationCode = confirmation_code
  )

  response <- POST(
    url = auth_url,
    add_headers(
      "X-Amz-Target" = "AWSCognitoIdentityProviderService.ConfirmSignUp",
      "Content-Type" = "application/x-amz-json-1.1"
    ),
    body = toJSON(body, auto_unbox = TRUE)
  )

  if (status_code(response) == 200) {
    return(list(success = TRUE, message = "Account verified successfully! You can now log in."))
  } else {
    response_content <- httr::content(response, "text", encoding = "UTF-8")
    error_info <- tryCatch({
      fromJSON(response_content)
    }, error = function(e) {
      # Fallback for non-JSON errors
      list(message = paste("Unknown API Error:", response_content))
    })

    # Print error to Lightsail logs for debugging
    print(paste("Cognito Confirmation Error:", status_code(response), "-", error_info$message))

    return(list(success = FALSE, message = error_info$message))
  }
}

# Initiate password reset
forgot_password_cognito <- function(username) {
  config <- get_cognito_config()
  auth_url <- paste0("https://cognito-idp.", config$region, ".amazonaws.com/")

  body <- list(
    ClientId = config$client_id,
    Username = username
  )

  response <- POST(
    url = auth_url,
    add_headers(
      "X-Amz-Target" = "AWSCognitoIdentityProviderService.ForgotPassword",
      "Content-Type" = "application/x-amz-json-1.1"
    ),
    body = toJSON(body, auto_unbox = TRUE)
  )

  if (status_code(response) == 200) {
    return(list(success = TRUE, message = "Password reset code sent to your email."))
  } else {
    response_content <- httr::content(response, "text", encoding = "UTF-8")
    error_info <- fromJSON(response_content)
    return(list(success = FALSE, message = error_info$message))
  }
}

# Confirm password reset
confirm_forgot_password_cognito <- function(username, confirmation_code, new_password) {
  config <- get_cognito_config()
  auth_url <- paste0("https://cognito-idp.", config$region, ".amazonaws.com/")

  body <- list(
    ClientId = config$client_id,
    Username = username,
    ConfirmationCode = confirmation_code,
    Password = new_password
  )

  response <- POST(
    url = auth_url,
    add_headers(
      "X-Amz-Target" = "AWSCognitoIdentityProviderService.ConfirmForgotPassword",
      "Content-Type" = "application/x-amz-json-1.1"
    ),
    body = toJSON(body, auto_unbox = TRUE)
  )

  if (status_code(response) == 200) {
    return(list(success = TRUE, message = "Password reset successful! You can now log in with your new password."))
  } else {
    response_content <- httr::content(response, "text", encoding = "UTF-8")
    error_info <- fromJSON(response_content)
    return(list(success = FALSE, message = error_info$message))
  }
}
