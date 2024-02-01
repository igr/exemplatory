package hotel

import arrow.core.Either
import arrow.core.left
import arrow.core.right
import java.util.*

/**
Starting point: total decomposition of the problem into functions and data.

Functions:
+ business functions, which are pure
+ data/repo functions, which are not pure
+ utility functions.

There are two types of data:
+ domain, business data; usually composition of other types
+ repo data, which is usually a single type. Contains the field of `Id` type.

This separation is important in the modeling, as repo data indicates the _owner_.
On the other hand, business data can be anything, and it may be composed with data
from various owners.

Each repo data type has its pair of functions:
+ first group is for 'query' (find*),
+ second group is for 'command' (store*, etc.)

There is no owner of functions at the beginning; all are free, without ownership.

 */

typealias Id = Int
typealias Value = String

data class DateRange(val value: Value)
data class RoomType(val id: Id)
data class DayPrice(val id: Id)
data class BookingChoice(val roomType: RoomType, val dayPrices: List<DayPrice>)

/** Requirement #1 */

internal fun findAvailableRoomTypes(dateRange: DateRange): List<RoomType> {
	TODO()
}

internal fun findDayPricesForRoomType(roomType: RoomType, dateRange: DateRange): List<DayPrice> {
	TODO()
}

fun search(dateRange: DateRange): List<BookingChoice> {
	return findAvailableRoomTypes(dateRange).map { roomType ->
		findDayPricesForRoomType(roomType, dateRange).let {
			BookingChoice(roomType, it)
		}
	}
}


/** Requirement #2 */

data class UserDetails(val value: Value)
data class Reservation(val id: Id, val roomType: RoomType, val dateRange: DateRange)
data class Booking(val id: Id, val customer: Customer, val reservation: Reservation)
data class Amount(val value: Value) {
	companion object {
		fun of(dayPrices: DayPrice): Amount {
			TODO()
		}
		fun total(amounts: List<Amount>): Amount {
			TODO()
		}
	}
}

data class Customer(val id: Id, val userDetails: UserDetails)

internal fun authorizePayment(customer: Customer, fee: Amount): Boolean {
	TODO()
}

internal fun findCancellationFee(): Amount {
	TODO()
}

internal fun sendEmail(customer: Customer, booking: Booking) {
	TODO()
}

internal fun createReservation(roomType: RoomType, dateRange: DateRange): Either<String, Reservation> {
	// changes the availability of rooms.
	// this has to be atomic operation; but this is not important for modeling!
	TODO()
}
internal fun findReservationPrices(reservation: Reservation): List<DayPrice> {
	TODO()
}

internal fun storeCustomer(userDetails: UserDetails): Customer {
	TODO()
}
internal fun createBooking(customer: Customer, reservation: Reservation): Booking {
	TODO()
}

fun book(userDetails: UserDetails, roomType: RoomType, dateRange: DateRange): Either<String, Booking> {
	val customer = storeCustomer(userDetails)
	val fee = findCancellationFee()
	if (!authorizePayment(customer, fee)) {
		return "No money".left()
	}

	val reservation = createReservation(roomType, dateRange)

	return with(reservation) {
		when (this) {
			is Either.Left -> "No room".left()
			is Either.Right -> {
				val booking = createBooking(customer, this.value)
				sendEmail(customer, booking)
				return booking.right()
			}
		}
	}
}

/** Requirement #3 */

internal fun findCustomer(userDetails: UserDetails): Optional<Customer> {
	TODO()
}
fun findBooking(customer: Customer): Optional<Booking> {
	// and other query options
	TODO()
}

fun authorizeBooking(booking: Booking) {
	val prices = findReservationPrices(booking.reservation).map { Amount.of(it) }
	authorizePayment(booking.customer, Amount.total(prices))
}

fun pickRoom(booking: Booking): ActiveBooking {
	val roomType = booking.reservation.roomType
	val room = reserveUnoccupiedRoom(roomType)
	return storeActiveBooking(booking, room)
}

data class Room(val id: Id)

data class ActiveBooking(val id: Id, val booking: Booking, val room: Room)

fun storeActiveBooking(booking: Booking, room: Room): ActiveBooking {
	TODO()
}

internal fun reserveUnoccupiedRoom(roomType: RoomType): Room {
	// we don't need to know date ranges, as any room of given type is ok
	// this also decreases the number of rooms
	TODO()
}

/** Requirements #4 */

internal fun findAllActiveBookings(): List<ActiveBooking> {
	TODO()
}

internal fun triggerEachDay() {
	findAllActiveBookings()
		.forEach {
			if (it.booking.reservation.dateRange.value == "last day")
				printTheBill(it)
			if (it.booking.reservation.dateRange.value == "today")
				checkout(it)
		}
}

internal fun printTheBill(activeBooking: ActiveBooking) {
	TODO()
}

internal fun deleteActiveBooking(activeBooking: ActiveBooking) {
	// add tasks for hotel personel
	// no time to do it now
	TODO()
}

internal fun checkout(activeBooking: ActiveBooking) {
	deleteActiveBooking(activeBooking)
	TODO()
}
