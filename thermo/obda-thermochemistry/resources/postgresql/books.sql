--
-- PostgreSQL database dump
--

-- Dumped from database version 12.3
-- Dumped by pg_dump version 12.3

-- Started on 2020-07-22 17:38:57

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- TOC entry 202 (class 1259 OID 16399)
-- Name: tb_books; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.tb_books (
    bk_code text NOT NULL,
    bk_title text
);


ALTER TABLE public.tb_books OWNER TO postgres;

--
-- TOC entry 2814 (class 0 OID 16399)
-- Dependencies: 202
-- Data for Name: tb_books; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.tb_books (bk_code, bk_title) FROM stdin;
1	book_title_1
2	book_title_3
3	book_title_3
4	book_title_4
\.


--
-- TOC entry 2687 (class 2606 OID 16406)
-- Name: tb_books tb_books_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.tb_books
    ADD CONSTRAINT tb_books_pkey PRIMARY KEY (bk_code);


-- Completed on 2020-07-22 17:38:57

--
-- PostgreSQL database dump complete
--

