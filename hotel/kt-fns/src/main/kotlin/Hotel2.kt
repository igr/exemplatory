package hotel

/**

Grouping the functions.

Grouping is done just by considering the inputs and the outputs of the functions.

There is an indication that this can be even done/proposed by statical analysis
of the signatures and their combinations!!

*/

// ROOM TYPES, AVAILABLE ROOMS, ROOMS
val findAvailableRoomTypes: (DateRange) -> List<RoomType> = TODO()
val reserveUnoccupiedRoom: (RoomType) -> Room

// RESERVATION (ROOM PRICES) - _maybe_ part of the same service as above
val findDayPricesForRoomType: (RoomType, DateRange) -> List<DayPrice>
val findReservationPrices: (Reservation) -> List<DayPrice>
val createReservation: (RoomType, DateRange) -> Reservation

// BOOKING
val createBooking: (Customer, Reservation) -> Booking
val findBooking: (Customer) -> Booking

// PAYMENT
val authorizePayment: (Customer, Amount) -> Boolean
val findCancellationFee: () -> Amount

// CHECKIN, ACTIVE BOOKING
val pickRoom: (Booking) -> ActiveBooking
val storeActiveBooking: (Booking, Room) -> ActiveBooking
val findAllActiveBookings: () -> List<ActiveBooking>
val deleteActiveBooking: (ActiveBooking) -> Unit

// CUSTOMER
val storeCustomer: (UserDetails) -> Customer
val findCustomer: (UserDetails) -> Customer

// GENERAL SERVICES
val sendEmail: (Customer, Booking) -> Unit
val triggerEachDay: () -> Unit
val printTheBill: (ActiveBooking) -> Unit



// (UI)
val search: (DateRange) -> List<BookingChoice>
val book: (UserDetails, RoomType, DateRange) -> Booking
val authorizeBooking: (Booking) -> Unit