-- phpMyAdmin SQL Dump
-- version 3.4.7
-- http://www.phpmyadmin.net
--
-- Хост: localhost
-- Время создания: Ноя 16 2011 г., 00:41
-- Версия сервера: 5.1.58
-- Версия PHP: 5.3.8

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- База данных: `a1`
--

-- --------------------------------------------------------

--
-- Структура таблицы `account`
--

CREATE TABLE IF NOT EXISTS `account` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `login` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `pass` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `email` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `cookie` blob NOT NULL,
  `time` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `online` int(1) NOT NULL,
  `last_char` int(11) NOT NULL,
  `status` tinyint(4) NOT NULL COMMENT 'статус регистрации (0- ок, 1-ждем мыла, 3-отказано)',
  `reg_id` varchar(255) COLLATE utf8_unicode_ci NOT NULL COMMENT 'ид регистрации',
  `reg_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=13 ;

--
-- Дамп данных таблицы `account`
--

INSERT INTO `account` (`id`, `login`, `pass`, `email`, `cookie`, `time`, `online`, `last_char`, `status`, `reg_id`, `reg_date`) VALUES
(1, 'ark', '123zxc', '', 0x004d722484e9dfb390bdf31f66b00c51, '2011-11-15 06:39:15', 0, 1, 0, '', '0000-00-00 00:00:00'),
(2, 'test', 'hgjhgjhgj', '', 0x31551a3ae59f0596d967b3f757662894, '0000-00-00 00:00:00', 0, 3, 0, '', '0000-00-00 00:00:00'),
(12, 'ark2', 'fgjhnbvn', 'ark@ark.su', 0x92eb5ffee6ae2fec3ad71c777531578f, '0000-00-00 00:00:00', 0, 20, 0, '0efda830a6f0cfb7b884e1c8de0b3ac9', '0000-00-00 00:00:00');

-- --------------------------------------------------------

--
-- Структура таблицы `buffs`
--

CREATE TABLE IF NOT EXISTS `buffs` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `target_id` int(10) unsigned NOT NULL,
  `type` int(10) unsigned NOT NULL,
  `duration` int(10) unsigned NOT NULL,
  `state` blob NOT NULL,
  PRIMARY KEY (`id`),
  KEY `target_id` (`target_id`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=121 ;

-- --------------------------------------------------------

--
-- Структура таблицы `char`
--

CREATE TABLE IF NOT EXISTS `char` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `account_id` int(11) NOT NULL,
  `access_level` int(10) unsigned NOT NULL DEFAULT '0',
  `name` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `x` int(11) NOT NULL,
  `y` int(11) NOT NULL,
  `l` tinyint(3) unsigned NOT NULL,
  `hp` int(11) NOT NULL,
  `shp` int(11) NOT NULL,
  `hhp` int(11) NOT NULL,
  `exp_nature` int(10) unsigned NOT NULL DEFAULT '0',
  `exp_industry` int(10) unsigned NOT NULL DEFAULT '0',
  `exp_combat` int(10) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=5 ;

--
-- Дамп данных таблицы `char`
--

INSERT INTO `char` (`id`, `account_id`, `access_level`, `name`, `x`, `y`, `l`, `hp`, `shp`, `hhp`, `exp_nature`, `exp_industry`, `exp_combat`) VALUES
(1, 1, 10000, '=arksu=', 185, 1925, 0, 0, 0, 0, 840, 342, 0),
(2, 1, 0, 'ark_char2', 166, 1866, 0, 0, 0, 0, 20, 7, 0),
(3, 1, 0, 'test1', 3064, 2268, 0, 0, 0, 0, 15, 5, 0),
(4, 1, 0, 'test2', 2337, 2377, 0, 0, 0, 0, 182, 79, 0);

-- --------------------------------------------------------

--
-- Структура таблицы `inventory`
--

CREATE TABLE IF NOT EXISTS `inventory` (
  `id` int(11) NOT NULL,
  `inv_id` int(11) NOT NULL,
  `type` smallint(11) unsigned NOT NULL COMMENT 'тип вещи',
  `q` smallint(11) unsigned NOT NULL DEFAULT '10' COMMENT 'качество',
  `amount` smallint(11) unsigned NOT NULL DEFAULT '0' COMMENT 'количество (литры)',
  `ticks_left` smallint(11) unsigned NOT NULL DEFAULT '0' COMMENT 'сколько осталось тиков',
  `ticks` smallint(11) unsigned NOT NULL DEFAULT '0' COMMENT 'всего тиков надо для завершения',
  `x` tinyint(11) unsigned NOT NULL COMMENT 'координата',
  `y` tinyint(11) unsigned NOT NULL COMMENT 'координата',
  `num` tinyint(4) unsigned NOT NULL DEFAULT '0' COMMENT 'стадия. доп циферка у вещи',
  PRIMARY KEY (`id`),
  KEY `inv_id` (`inv_id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

-- --------------------------------------------------------

--
-- Структура таблицы `log_login`
--

CREATE TABLE IF NOT EXISTS `log_login` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `account_id` int(11) NOT NULL,
  `time` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `result` tinyint(4) NOT NULL,
  `login` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=50 ;

--
-- Дамп данных таблицы `log_login`
--

INSERT INTO `log_login` (`id`, `account_id`, `time`, `result`, `login`) VALUES

(49, 1, '2011-11-15 06:39:15', 0, 'ark');

-- --------------------------------------------------------

--
-- Структура таблицы `server`
--

CREATE TABLE IF NOT EXISTS `server` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `val_name` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `val` int(11) NOT NULL,
  `time` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`val_name`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=6 ;

--
-- Дамп данных таблицы `server`
--

INSERT INTO `server` (`id`, `val_name`, `val`, `time`) VALUES
(1, 'freeid', 1015026, '2011-11-10 02:13:32'),
(2, 'online_count', 0, '2011-11-15 14:24:37'),
(4, 'crawler_0', 295, '2011-11-13 05:36:47'),
(5, 'global_time', 77358, '2011-11-15 14:24:41');

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
