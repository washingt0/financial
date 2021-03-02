CREATE ROLE financial WITH NOSUPERUSER INHERIT NOCREATEROLE NOCREATEDB LOGIN NOREPLICATION NOBYPASSRLS ENCRYPTED PASSWORD 'development';

CREATE DATABASE financial;

\c financial

GRANT USAGE ON SCHEMA public TO financial;

CREATE OR REPLACE FUNCTION public.tf_set_updated_at()
RETURNS TRIGGER AS
$$
    BEGIN
        NEW.updated_at := now();
        RETURN NEW;
    END;
$$
LANGUAGE 'plpgsql';

ALTER FUNCTION public.tf_set_updated_at OWNER TO lab;

GRANT EXECUTE ON FUNCTION public.tf_set_updated_at TO financial;

REVOKE ALL ON FUNCTION public.tf_set_updated_at FROM public;

CREATE TABLE public.t_audit (
    id UUID PRIMARY KEY NOT NULL DEFAULT gen_random_uuid(),
    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
    run_at TIMESTAMP NOT NULL DEFAULT clock_timestamp(),
    database_user TEXT NOT NULL,
    application_user TEXT NOT NULL,
    origin_ip INET NOT NULL,
    remote_ip INET,
    local_ip INET,
    user_agent TEXT,
    request_id UUID,
    handler TEXT,
    schema TEXT NOT NULL,
    "table" TEXT NOT NULL,
    operation TEXT NOT NULL,
    query TEXT NOT NULL,
    old JSONB,
    new JSONB
);

ALTER TABLE public.t_audit OWNER TO lab;

REVOKE ALL ON TABLE public.t_audit FROM public;

GRANT INSERT ON TABLE public.t_audit TO public;

CREATE OR REPLACE FUNCTION public.tf_add_audit()
RETURNS TRIGGER AS
$$
    DECLARE
        _old JSONB := NULL;
        _new JSONB := NULL;

        _user_id    TEXT := NULL;
        
        _request_id TEXT := NULL;
        _remote_ip INET  := NULL;
        _local_ip INET   := NULL;
        _user_agent TEXT := NULL;
        _handler TEXT    := NULL;

        _super      BOOLEAN := FALSE;
    BEGIN
        IF TG_OP = 'INSERT' THEN
            _new := to_jsonb(NEW.*);
        END IF;

        IF TG_OP = 'UPDATE' THEN
            _old := to_jsonb(OLD.*);
            _new := to_jsonb(NEW.*);
        END IF;

        IF TG_OP = 'DELETE' THEN
            _old := to_jsonb(OLD.*);
        END IF;

        BEGIN
            SHOW application.handler    INTO _handler;
            SHOW application.remote_ip  INTO _remote_ip;
            SHOW application.local_ip   INTO _local_ip;
            SHOW application.user_agent INTO _user_agent;
        EXCEPTION WHEN OTHERS THEN
        END;

        BEGIN
            SHOW application.user_id    INTO _user_id;
            SHOW application.request_id INTO _request_id;
        EXCEPTION WHEN OTHERS THEN
            SHOW IS_SUPERUSER INTO _super;
            IF _super THEN
                _user_id := 'SUPER_USER';
                _request_id := NULL;
            ELSE
                RAISE EXCEPTION assert_failure USING HINT = 'unable to perform operations without the associated user/request';
            END IF;
        END;

        INSERT INTO public.t_audit(database_user, application_user, origin_ip, remote_ip, local_ip, user_agent, handler, schema, "table", operation, query, request_id, old, new)
        VALUES (CURRENT_USER, _user_id,  COALESCE(inet_client_addr(), '127.0.0.1'::INET), _remote_ip, _local_ip, _user_agent, _handler,  TG_TABLE_SCHEMA, TG_TABLE_NAME, TG_OP, current_query(), _request_id::UUID, _old, _new);

        IF TG_OP = 'INSERT' OR TG_OP = 'UPDATE' THEN
            RETURN NEW;
        END IF;

        IF TG_OP = 'DELETE' THEN
            RETURN OLD;
        END IF;

        RETURN NULL;
    END;
$$
LANGUAGE 'plpgsql';

ALTER FUNCTION public.tf_add_audit() OWNER TO lab;

REVOKE ALL ON FUNCTION public.tf_add_audit() FROM public;

GRANT EXECUTE ON FUNCTION public.tf_add_audit() TO public;

CREATE TABLE public.t_migration (
    id UUID PRIMARY KEY NOT NULL DEFAULT gen_random_uuid(),
    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP,
    deleted_at TIMESTAMP,
    name TEXT NOT NULL CHECK(char_length(name) BETWEEN 4 AND 128),
    rolled_back BOOLEAN NOT NULL DEFAULT FALSE
);

CREATE TRIGGER set_updated_at
BEFORE UPDATE ON public.t_migration
FOR EACH ROW WHEN (OLD.* IS DISTINCT FROM  NEW.*)
EXECUTE PROCEDURE public.tf_set_updated_at();

ALTER TABLE public.t_migration OWNER TO lab;

GRANT SELECT ON TABLE public.t_migration TO financial;

REVOKE ALL ON TABLE public.t_migration FROM public;

CREATE TABLE public.t_outgoing_message (
    id UUID PRIMARY KEY NOT NULL DEFAULT gen_random_uuid(),
    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP,
    deleted_at TIMESTAMP,
    sent_at TIMESTAMP,
    error TEXT,
    queue TEXT NOT NULL,
    payload JSONB NOT NULL
);

CREATE TRIGGER set_updated_at
BEFORE UPDATE ON public.t_outgoing_message
FOR EACH ROW WHEN (OLD.* IS DISTINCT FROM  NEW.*)
EXECUTE PROCEDURE public.tf_set_updated_at();

CREATE TRIGGER add_audit
BEFORE UPDATE OR DELETE OR INSERT ON public.t_outgoing_message
FOR EACH ROW
EXECUTE PROCEDURE public.tf_add_audit();

ALTER TABLE public.t_outgoing_message OWNER TO lab;

GRANT SELECT, INSERT, UPDATE ON TABLE public.t_outgoing_message TO financial;

REVOKE ALL ON TABLE public.t_outgoing_message FROM public;

CREATE TABLE public.t_country (
    id UUID PRIMARY KEY NOT NULL DEFAULT gen_random_uuid(),
    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP,
    deleted_at TIMESTAMP,
    name TEXT CHECK(char_length(name) BETWEEN 3 AND 64),
    code TEXT CHECK(char_length(code) = 3),
    currency TEXT check(char_length(currency) < 5),
    timezone TEXT check(char_length(timezone) < 50)
);

CREATE UNIQUE INDEX unique_country_not_deleted ON public.t_country (code) WHERE deleted_at IS NULL;

CREATE TRIGGER set_updated_at
BEFORE UPDATE ON public.t_country
FOR EACH ROW WHEN (OLD.* IS DISTINCT FROM  NEW.*)
EXECUTE PROCEDURE public.tf_set_updated_at();

CREATE TRIGGER add_audit
BEFORE UPDATE OR DELETE OR INSERT ON public.t_country
FOR EACH ROW
EXECUTE PROCEDURE public.tf_add_audit();

ALTER TABLE public.t_country OWNER TO lab;

GRANT SELECT, INSERT, UPDATE ON TABLE public.t_country TO financial;

REVOKE ALL ON TABLE public.t_country FROM public;

CREATE TABLE public.t_bank (
    id UUID PRIMARY KEY NOT NULL DEFAULT gen_random_uuid(),
    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP,
    deleted_at TIMESTAMP,
    name TEXT CHECK(char_length(name) BETWEEN 3 AND 32),
    full_name TEXT CHECK(char_length(full_name) BETWEEN 3 AND 128),
    country_id UUID NOT NULL REFERENCES public.t_country(id),
    identifier TEXT CHECK(char_length(identifier) < 32),
    code TEXT CHECK(char_length(code) < 16)
);

CREATE UNIQUE INDEX unique_bank_not_deleted ON public.t_bank (identifier, country_id) WHERE deleted_at IS NULL;

CREATE TRIGGER set_updated_at
BEFORE UPDATE ON public.t_bank
FOR EACH ROW WHEN (OLD.* IS DISTINCT FROM  NEW.*)
EXECUTE PROCEDURE public.tf_set_updated_at();

CREATE TRIGGER add_audit
BEFORE UPDATE OR DELETE OR INSERT ON public.t_bank
FOR EACH ROW
EXECUTE PROCEDURE public.tf_add_audit();

ALTER TABLE public.t_bank OWNER TO lab;

GRANT SELECT, INSERT, UPDATE ON TABLE public.t_bank TO financial;

REVOKE ALL ON TABLE public.t_bank FROM public;

CREATE TABLE public.t_account (
    id UUID PRIMARY KEY NOT NULL DEFAULT gen_random_uuid(),
    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP,
    deleted_at TIMESTAMP,
    bank_id UUID NOT NULL REFERENCES public.t_bank(id),
    user_id UUID NOT NULL,
    nickname TEXT NOT NULL CHECK(char_length(nickname) BETWEEN 3 AND 16)
);

CREATE UNIQUE INDEX unique_account_not_deleted ON public.t_account (bank_id, user_id, nickname) WHERE deleted_at IS NULL;

CREATE TRIGGER set_updated_at
BEFORE UPDATE ON public.t_account
FOR EACH ROW WHEN (OLD.* IS DISTINCT FROM  NEW.*)
EXECUTE PROCEDURE public.tf_set_updated_at();

CREATE TRIGGER add_audit
BEFORE UPDATE OR DELETE OR INSERT ON public.t_account
FOR EACH ROW
EXECUTE PROCEDURE public.tf_add_audit();

ALTER TABLE public.t_account OWNER TO lab;

GRANT SELECT, INSERT, UPDATE ON TABLE public.t_account TO financial;

REVOKE ALL ON TABLE public.t_account FROM public;

SET application.user_id TO 'migration';

INSERT INTO public.t_migration (name) VALUES ('0000');
