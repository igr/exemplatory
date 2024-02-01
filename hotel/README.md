# Hotel 🏨

Determine _boundaries_ from the described Hotel management requirements.

N.B. we don't look for running code, just boundaries.

## Requirements

Rules (to simplify):

+ 1 guest per reservation
+ 1 room per reservation

## Req #1: Search for availability

+ Users searches for availability for date range.
+ The search result contains a list of room types available for the date range.

## Req #2: Make a booking

+ User selects one room type from the search results.
+ Selected room type shows information, including the summary of the charges. Each day has its price.
+ User may continue with the booking.

## Req #2.1: Continue booking

+ User enters names and finishes the booking.

Booking steps:

1. First, authorize the cancellation fee
2. If successful, see if a room is still available (multiple users using the same site)
3. If not, release cancellation fee authorization.

+ Confirmation email should be sent
+ Booking results are shown on the next screen.

Domain rules:

+ Room number is not allocated at time of booking.
+ Individual rooms are not "locked" while booking is in progress.
+ Capacity pf the Hotel must be respected.

## Req #3: Front desk Check-In

+ Check-in: find booking by last name to verify.
+ Authorize payment for full stay.
+ Pick a physical room for booked room type.

### Req #4: Front desk Check-out

+ Night before check-out, print the bill.
+ Guest leaves the room, hotel personnel verifies.
+ Client card is charged for extras if there is any.

## Solutions

⭐️ [kt-fns](kt-fns) (by @igr)

> Idea of mine. The process of grouping is done in 2 steps. First, we write down all the functions signatures and data types from the requirements. Put as less details as possible, and no implementation. Then, we start grouping. Just by observing the input and output types, we can see that some functions are related to each other. We can group them together. We don't even have to take into the account the function names, just types. There is no one way to do it, however, there might be a statistical solution that can be applied to this problem.
