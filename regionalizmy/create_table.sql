
CREATE TABLE regionalizmy
(
	`timestamp` TIMESTAMP NOT NULL,
	`majonez` VARCHAR(15) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL,
	`ketchup` VARCHAR(15) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL,
	`ekstraklasa` VARCHAR(25) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL,
	`long` DOUBLE NOT NULL,
	`lat` DOUBLE NOT NULL
) ENGINE = InnoDB CHARSET=utf8 COLLATE utf8_general_ci;

