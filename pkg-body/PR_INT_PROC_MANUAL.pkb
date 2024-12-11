CREATE OR REPLACE PACKAGE BODY ACSELPRB.PR_INT_PROC_MANUAL AS

/*
REVISIONES(NUEVA VERSION DIMENSIONS:
  Version       Fecha        Autor          Incidencia        Cambio realizado
--------------------------------------------------------------------------------
    1.4       11/12/2024     kron-fm          ggfgfgf         PRUEBA
    1.3       20/08/2023     COTECSA-MX     COLWSSEGUR-6414   Rehabilitacion Stock. Cambiar Operaciones ANU sin Monto a MOD.
    1.2       20/08/2023     COTECSA-MX     COLWSSEGUR-6414   Rehabilitacion Stock. Mejora Validacion para Reverso de Oblig. por Devolucion
    1.1       11/08/2023     COTECSA-MX     COLWSSEGUR-6414   Rehabilitacion Stock.


REVISIONES:
  Version       Fecha        Autor          Incidencia      Cambio realizado
--------------------------------------------------------------------------------
    1.1       28/06/2023     COTECSA-MX        -            Paquete de Utilidades para Polizas. (Ajustes).
    1.0       28/06/2023     COTECSA-MX        -            Paquete de Utilidades para Polizas.
*/

PROCEDURE   ANULAR_OBLIGACION ( pNumOblig NUMBER ) IS

cUsrACT     USUARIO.CodUsr%TYPE             := USER;
cUsrACX     USUARIO.CodUsr%TYPE             := 'ACSELPRB';
dFecMov     HIST_OBLIGACION.FechaMod%TYPE   := TRUNC(SYSDATE);
nIdeComp    COMPROBANTE.IdeComp%TYPE;

BEGIN

    PR_OBLIGACION.ANULAR ( pNumOblig );

    UPDATE  HIST_OBLIGACION OB
        SET UsuarioMod      = cUsrACX
    WHERE   OB.NumOblig   = pNumOblig
    AND     OB.FechaMod   = dFecMov
    AND     OB.UsuarioMod = cUsrACT;

    BEGIN
        SELECT MAX(CO.IdeComp)
        INTO   nIdeComp
        FROM   COMPROBANTE CO
        WHERE  CO.NumOblig = pNumOblig
        AND    CO.TipComp  = '020'
        AND    CO.FecMov   = dFecMov;
    EXCEPTION
        WHEN OTHERS THEN
            nIdeComp  := 0;
    END;

    IF nIdeComp != 0 THEN

        UPDATE  MOV_DEFINITIVO MD
            SET MD.Usuario    = cUsrACX
        WHERE   MD.IdeComp = nIdeComp
        AND     MD.Usuario = cUsrACT;

    END IF;

END;

PROCEDURE   ANULAR_REL_ING ( pNumRelIng NUMBER ) IS

nNumOperIng     REL_ING.NumOper%TYPE;
dFecAnulRelIng  REL_ING.FecAnulRelIng%TYPE;

cUsrACT         USUARIO.CodUsr%TYPE     := USER;
cUsrACX         USUARIO.CodUsr%TYPE     := 'ACSELPRB';

CURSOR C_COMPROB IS
    SELECT  CO.IdeComp
    FROM    COMPROBANTE CO
    WHERE   CO.NumOper  = nNumOperIng
    AND     CO.TipComp  = '036'
    AND     CO.FecMov   = dFecAnulRelIng;

BEGIN

    BEGIN
        SELECT  RI.NumOper
        INTO    nNumOperIng
        FROM    REL_ING RI
        WHERE   RI.NumRelIng = pNumRelIng;
    EXCEPTION
        WHEN OTHERS THEN
            nNumOperIng     := NULL;
    END;

    dFecAnulRelIng  := TRUNC(SYSDATE);

    UPDATE  REL_ING RI
        SET RI.CodMotvAnul  = '0004',
            RI.TextAnul     = 'REHAB POLWEB'
    WHERE   RI.NumRelIng = pNumRelIng;

    PR_ANULAR_REL_ING.ANULAR_INGRESO(pNumRelIng);
    PR_MOV_DEFINITIVO.nNumRelIng := NULL;

    BEGIN
        SELECT  RI.FecAnulRelIng
        INTO    dFecAnulRelIng
        FROM    REL_ING RI
        WHERE   RI.NumRelIng = pNumRelIng;
    EXCEPTION
        WHEN OTHERS THEN
            dFecAnulRelIng  := TRUNC(SYSDATE);
    END;

    FOR COM IN C_COMPROB LOOP

        UPDATE  MOV_DEFINITIVO MD
            SET MD.Usuario    = cUsrACX
        WHERE   MD.IdeComp = COM.IdeComp
        AND     MD.Usuario = cUsrACT;

    END LOOP;

END;

PROCEDURE   BORRAR_MOD_COBERT  ( pIdePol NUMBER,pNumCert NUMBER,pNumOper NUMBER ) IS

CURSOR C_MOD_COBERT IS
    SELECT  RE.NumOper,RE.IdeMovPrima,MC.IdeCobert,MC.NumMod,RE.IdeRec
    FROM    RECIBO RE,MOD_COBERT MC
    WHERE   RE.IdeMovPrima  = MC.IdeMovPrima
    AND     RE.IdePol       = pIdePol
    AND     RE.NumCert      = pNumCert
    AND     RE.NumOper      = pNumOper;

BEGIN

    FOR MOC IN C_MOD_COBERT LOOP

        DELETE FROM MOD_COBERT MC WHERE MC.IdeMovPrima = MOC.IdeMovPrima;
        DELETE FROM TRANS_COBERT TC WHERE TC.IdeCobert = MOC.IdeCobert AND TC.NumMod = MOC.NumMod;
        DELETE FROM MOD_COBERT_IVA_ADIC TC WHERE TC.IdeCobert = MOC.IdeCobert AND TC.NumMod = MOC.NumMod; --COLWSSEGUR-6414--
        DELETE FROM GEN_REA GR WHERE GR.IdePol = pIdePol AND GR.NumCert = pNumCert AND GR.IdeRec = MOC.IdeRec;

    END LOOP;

END;

FUNCTION    APLICAR_COBRO_WEB ( pIdeFact NUMBER,pMonto NUMBER ) RETURN NUMBER IS


nIdePol             POLIZA.IdePol%TYPE;
nNumOper            FACTURA.NumOper%TYPE;
cStsFact            FACTURA.StsFact%TYPE;
nSldoFactMoneda     FACTURA.SldoFactMoneda%TYPE;
nMtoFact            FACTURA.MtoFactMoneda%TYPE;
dFecStsRelIng       REL_ING.FecStsRelIng%TYPE;
nNumRelIng          REL_ING.NumRelIng%TYPE;
nNumOperIng         REL_ING.NumOper%TYPE;
cUsrACT             USUARIO.CodUsr%TYPE     := USER;
cUsrACX             USUARIO.CodUsr%TYPE     := 'ACSELPRB';

CURSOR C_COMPROB IS
    SELECT  CO.IdeComp
    FROM    COMPROBANTE CO
    WHERE   CO.NumOper  = nNumOperIng
    AND     CO.TipComp  = '002'
    AND     CO.FecMov   = dFecStsRelIng;

BEGIN

    BEGIN
        SELECT  FA.StsFact,NVL(FA.SldoFactMoneda,0)
        INTO    cStsFact,nSldoFactMoneda
        FROM    FACTURA FA
        WHERE   FA.IdeFact  = pIdeFact;
    EXCEPTION
        WHEN OTHERS THEN
            cStsFact        := NULL;
            nSldoFactMoneda := NULL;
    END;

    IF cStsFact != 'ACT' OR nSldoFactMoneda <= 0 THEN
        RAISE_APPLICATION_ERROR (-20100,'ERROR: La Factura no esta Activa o tiene Saldo para Cobrar');
    END IF;

    IF NVL(pMonto,0) > nSldoFactMoneda THEN
        nMtoFact    := nSldoFactMoneda;
    ELSE
        nMtoFact    := pMonto;
    END IF;

    COBRAR_ALTAMIRA_INT ( pIdeFact,NULL,'1','Cobro IdePol: ' ||nIdePol,NULL,nMtoFact,nNumRelIng );

    UPDATE  REL_ING RI
        SET RI.CodCajero  = CASE WHEN RI.CodCajero = cUsrACT THEN cUsrACX ELSE RI.CodCajero END
    WHERE   RI.NumRelIng  = nNumRelIng
    RETURN RI.FecStsRelIng,RI.NumOper INTO dFecStsRelIng,nNumOperIng;

    FOR COM IN C_COMPROB LOOP

        UPDATE  MOV_DEFINITIVO MD
            SET MD.Usuario    = cUsrACX
        WHERE   MD.IdeComp = COM.IdeComp
        AND     MD.Usuario = cUsrACT;

    END LOOP;

RETURN nNumRelIng;
END;

PROCEDURE REVERTIR_RECAUDO_POLWEB ( pIdePol NUMBER,pNumCert NUMBER,pModCobert VARCHAR2 ) IS

nNumCert    CERTIFICADO.NumCert%TYPE    := 1;
nNumOper    OBLIGACION.NumOper%TYPE;


CURSOR C_ANUL_WEB IS
    SELECT  RE.*,OB.StsOblig
    FROM    REC_POL_WEB_ANULADA RE,OBLIGACION OB
    WHERE   RE.NumOblig = OB.NumOblig
    AND     RE.IdePol   = pIdePol;

BEGIN

    IF pNumCert IS NOT NULL THEN
        nNumCert := pNumCert;
    END IF;

    FOR REP IN C_ANUL_WEB LOOP

        --|| Si Esta Anulada la dejamos Pasar para Borrar los ModCobert ||--
        IF REP.StsOblig NOT IN ('ANU','ACT') THEN
            RAISE_APPLICATION_ERROR(-20100,'ERROR: La Obligacion de Reverso no esta Activa, no es Posible Continuar. ');
        END IF;

        IF REP.StsOblig = 'ACT' THEN
            PR_INT_PROC_MANUAL.ANULAR_OBLIGACION (REP.NumOblig);
        END IF;


        IF pModCobert = 'S' THEN

            BEGIN
                SELECT  OB.NumOper
                INTO    nNumOper
                FROM    OBLIGACION OB
                WHERE   OB.NumOblig = REP.NumOblig;
            EXCEPTION
                WHEN OTHERS THEN
                   RAISE_APPLICATION_ERROR(-20100,'ERROR: Consulta OperOblig,(E)> '||SQLERRM);
            END;

            PR_INT_PROC_MANUAL.BORRAR_MOD_COBERT ( pIdePol,nNumCert,nNumOper );

        END IF;

        UPDATE  REC_POL_WEB_ANULADA RE
            SET RE.StsRec   = 'ANU',
                RE.Error    = 'Devolucion Reversa por Generación Incorrecta'
        WHERE   RE.NumOblig = REP.NumOblig;

    END LOOP;

END;

PROCEDURE   REVERTIR_OBLIG_DEVOLUCION ( pIdePol NUMBER,pNumCert NUMBER,pModCobert VARCHAR2 ) IS

nNumCert    CERTIFICADO.NumCert%TYPE    := 1;
nNumOper    OBLIGACION.NumOper%TYPE;
nNumOblig   OBLIGACION.NumOblig%TYPE;
cStsOblig   OBLIGACION.StsOblig%TYPE;

BEGIN

    IF pNumCert IS NOT NULL THEN
        nNumCert := pNumCert;
    END IF;

    BEGIN
        SELECT OB.NumOblig,OB.StsOblig
        INTO   nNumOblig,cStsOblig
        FROM   OPER_POL OP,OBLIGACION OB,DET_OBLIG DE
        WHERE  OP.NumOper     = OB.NumOper
        AND    OB.NumOblig    = DE.NumOblig
        AND    OP.IdePol      = pIdePol
        AND    OP.NumCert     = nNumCert
        AND    OP.TipoOp      = 'ANU'
        AND    DE.CodClaEgre  = 'DEVOLU'
        AND    DE.CodCptoEgre = 'DEVOLU';

    EXCEPTION
        WHEN OTHERS THEN
            nNumOblig   := NULL;
            cStsOblig   := NULL;
    END;

    IF cStsOblig NOT IN ('ACT','ANU') THEN
        RAISE_APPLICATION_ERROR(-20100,'ERROR: La Obligacion de Devolución no esta Activa, no es Posible Continuar. ');
    END IF;

    IF cStsOblig = 'ACT' THEN
       PR_INT_PROC_MANUAL.ANULAR_OBLIGACION (nNumOblig);
    END IF;

     --<<COLWSSEGUR-6414--
    IF pModCobert = 'S' THEN

        BEGIN
            SELECT  OB.NumOper
            INTO    nNumOper
            FROM    OBLIGACION OB
            WHERE   OB.NumOblig = nNumOblig;
        EXCEPTION
            WHEN NO_DATA_FOUND THEN
               nNumOper := 0;
            WHEN OTHERS THEN
               RAISE_APPLICATION_ERROR(-20100,'ERROR: Consulta OperOblig,(E)> '||SQLERRM);
        END;

        IF NVL(nNumOper,0) != 0 THEN
            PR_INT_PROC_MANUAL.BORRAR_MOD_COBERT ( pIdePol,nNumCert,nNumOper );
        END IF;

        --|| BORRAR MOVIMIENTOS QUE NO FACTURARON PRIMA ||--
        DELETE  FROM MOD_COBERT MC
        WHERE   MC.IdePol   = pIdePol
        AND     MC.NumCert  = nNumCert
        AND     MC.NumMod  != 1
        AND     MC.IdeMovPrima IS NULL;

    END IF;
    -->>COLWSSEGUR-6414--
END;

PROCEDURE   REVERTIR_COMPENSACION ( pIdePol NUMBER,pNumCert NUMBER ) IS

cStsOblig       OBLIGACION.StsOblig%TYPE;
nObligacion     NUMBER(1);

CURSOR C_REL_ING IS
    SELECT RI.NumRelIng
    FROM   OPER_POL OP,FACTURA FA,
           REL_ING RI, DOC_ING DI
    WHERE  OP.NumOper    = FA.NumOper
    AND    FA.NumRelIng  = RI.NumRelIng
    AND    RI.NumRelIng  = DI.NumRelIng
    AND    OP.IdePol     = pIdePol
    AND    OP.NumCert    = pNumCert
    AND    RI.StsRelIng  = 'ACT'
    AND    DI.TipoDocIng = 'CAO';

CURSOR RELING_OBLIG ( tNumRelIng NUMBER )  IS
    SELECT  NumOblig
    FROM    REL_ING_OBLIG RO,REL_ING RI
    WHERE   RO.NumRelIng    = RI.NumReling
    AND     RI.StsReling    = 'ANU'
    AND     RI.NumRelIng    = tNumRelIng
    AND     RO.NumRelIng    = tNumRelIng;

BEGIN

    FOR REL IN C_REL_ING LOOP

        PR_INT_PROC_MANUAL.ANULAR_REL_ING (REL.NumRelIng);

        FOR RIO IN RELING_OBLIG (REL.NumRelIng) LOOP

            BEGIN
                SELECT  DISTINCT 1,OB.StsOblig
                INTO    nObligacion,cStsOblig
                FROM    OBLIGACION OB,DET_OBLIG DE
                WHERE   OB.NumOblig    = RIO.NumOblig
                AND     OB.NumOblig    = DE.NumOblig
                AND     DE.CodClaEgre  = 'DEVOLU'
                AND     DE.CodCptoEgre = 'DISMIN';
            EXCEPTION
                WHEN NO_DATA_FOUND THEN
                    nObligacion := 0;
                WHEN OTHERS THEN
                    RAISE_APPLICATION_ERROR (-20101,'Error al Buscar Datos de la Obligacion := '||SQLERRM);
            END;

            IF nObligacion = 1 THEN
                IF cStsOblig = 'ACT' THEN
                    PR_INT_PROC_MANUAL.ANULAR_OBLIGACION (RIO.NumOblig);
                END IF;

                DELETE FROM REL_OBLACR_FRAC RE WHERE RE.IdePol = pIdePol AND RE.NumOblig = RIO.NumOblig;

            END IF;

        END LOOP;

    END LOOP;

END;

PROCEDURE   REHABILITAR_POL_SIN_OPERPOL ( pIdePol NUMBER ) IS

nNumModAseg     MOD_ASEG.NumMod%TYPE;
nNumModBien     MOD_BIEN_CERT.NumMod%TYPE;
nNumCert        CERTIFICADO.NumCert%TYPE    := 1;

CURSOR C_CERT_RAMO IS
    SELECT  CR.*
    FROM    CERT_RAMO CR
    WHERE   CR.IdePol       = pIdePol
    AND     CR.NumCert      = nNumCert
    AND     CR.StsCertRamo  IN ('ANU','EXC');

CURSOR C_CERT IS
    SELECT  CE.*
    FROM    CERTIFICADO CE
    WHERE   CE.IdePol   = pIdePol
    AND     CE.NumCert  = nNumCert
    AND     CE.StsCert  IN ('ANU','EXC');

CURSOR C_DIREC_RIESGO IS
    SELECT MD.*
    FROM   MOD_DIREC_RIESGO_CERT MD
    WHERE  MD.IdePol  = pIdePol
    AND    MD.NumCert = nNumCert
    AND    MD.StsDir  IN ('ANU','EXC');

CURSOR C_ASEGURADO ( tCodPlan VARCHAR2,tRevPlan VARCHAR2,tCodRamoCert VARCHAR2 ) IS
    SELECT AG.IdeAseg
    FROM   ASEGURADO AG
    WHERE  AG.IdePol      = pIdePol
    AND    AG.NumCert     = nNumCert
    AND    AG.CodPlan     = tCodPlan
    AND    AG.RevPlan     = tRevPlan
    AND    AG.CodRamoCert = tCodRamoCert
    AND    AG.StsAseg     IN ('ANU','EXC');

CURSOR C_COBERT_ASEG (tIdeAseg NUMBER) IS
    SELECT CA.IdeCobert
    FROM   COBERT_ASEG CA
    WHERE  CA.IdeAseg      = tIdeAseg
    AND    CA.StsCobert    IN ('ANU','EXC');

CURSOR C_BIEN_CERT ( tCodPlan VARCHAR2,tRevPlan VARCHAR2,tCodRamoCert VARCHAR2 ) IS
    SELECT BC.IdeBien
    FROM   BIEN_CERT BC
    WHERE  BC.IdePol      = pIdePol
    AND    BC.NumCert     = nNumCert
    AND    BC.CodPlan     = tCodPlan
    AND    BC.RevPlan     = tRevPlan
    AND    BC.CodRamoCert = tCodRamoCert
    AND    BC.StsBien     IN ('ANU','EXC');

CURSOR C_COBERT_BIEN ( tIdeBien NUMBER ) IS
    SELECT CB.IdeCobert
    FROM   COBERT_BIEN CB
    WHERE  CB.IdeBien      = tIdeBien
    AND    CB.StsCobert    IN ('ANU','EXC');

CURSOR C_COBERT_CERT ( tCodPlan VARCHAR2,tRevPlan VARCHAR2,tCodRamoCert VARCHAR2 ) IS
    SELECT CC.IdeCobert
    FROM   COBERT_CERT CC
    WHERE  CC.IdePol      = pIdePol
    AND    CC.NumCert     = nNumCert
    AND    CC.CodPlan     = tCodPlan
    AND    CC.RevPlan     = tRevPlan
    AND    CC.CodRamoCert = tCodRamoCert
    AND    CC.StsCobert    IN ('ANU','EXC');

BEGIN

    FOR RAM IN C_CERT_RAMO LOOP

        --|| ASEGURADOS ||--
        FOR ASE IN C_ASEGURADO ( RAM.CodPlan,RAM.RevPlan,RAM.CodRamoCert ) LOOP

            FOR COA IN C_COBERT_ASEG ( ASE.IdeAseg ) LOOP

                UPDATE  MOD_COBERT MC
                SET     MC.StsModCobert = 'ACT'
                WHERE   MC.IdeCobert = COA.IdeCobert;

                UPDATE  COBERT_ASEG CA
                SET     CA.StsCobert    = 'ACT',
                        CA.FecExc       = NULL,
                        CA.CodMotvExc   = NULL,
                        CA.TextMotvExc  = NULL
                WHERE   CA.IdeCobert = COA.IdeCobert;

                PR_COBERT_ASEG.ACT_MOD (COA.IdeCobert,1);

            END LOOP;

            BEGIN
                SELECT MAX (MA.NumMod)
                INTO   nNumModAseg
                FROM   MOD_ASEG MA
                WHERE  MA.IdeAseg    = ASE.IdeAseg
                AND    MA.StsModAseg = 'ANU'
                GROUP BY IdePol,NumCert;
            EXCEPTION
                WHEN OTHERS THEN
                    nNumModAseg := NULL;
                    RAISE_APPLICATION_ERROR (-20100,'ERROR: MOD_ASEG,(E)> '||SQLERRM);
            END;

            UPDATE  MOD_ASEG MA
                SET MA.StsModAseg   = 'ACT'
            WHERE   MA.IdeAseg = ASE.IdeAseg
            AND     MA.NumMod = nNumModAseg;

            UPDATE  ASEGURADO AG
                SET AG.StsAseg      = 'ACT',
                    AG.FecExc       = NULL
            WHERE   AG.IdeAseg = ASE.IdeAseg;

            UPDATE  BENEF_ASEG BA
                SET BA.StsBen      = 'ACT',
                    BA.FecExc      = NULL
            WHERE   BA.IdeAseg = ASE.IdeAseg;

        END LOOP;

        --|| BIENES ||--
        FOR BIE IN C_BIEN_CERT ( RAM.CodPlan,RAM.RevPlan,RAM.CodRamoCert ) LOOP

            FOR COB IN C_COBERT_BIEN ( BIE.IdeBien ) LOOP

                UPDATE  MOD_COBERT MC
                SET     MC.StsModCobert = 'ACT'
                WHERE   MC.IdeCobert = COB.IdeCobert;

                UPDATE  COBERT_BIEN CB
                SET     CB.StsCobert    = 'ACT',
                        CB.FecExc       = NULL,
                        CB.CodMotvExc   = NULL,
                        CB.TextMotvExc  = NULL
                WHERE   CB.IdeCobert = COB.IdeCobert;

                PR_COBERT_BIEN.ACT_MOD ( COB.IdeCobert,1 );

            END LOOP;

            BEGIN
                SELECT MAX (MB.NumMod)
                INTO   nNumModBien
                FROM   MOD_BIEN_CERT MB
                WHERE  MB.IdeBien    = BIE.IdeBien
                AND    MB.StsModBien IN ('ANU','EXC')
                GROUP BY MB.IdePol,MB.NumCert;
            EXCEPTION
                WHEN OTHERS THEN
                    nNumModAseg := NULL;
                    RAISE_APPLICATION_ERROR (-20100,'ERROR: MOD_ASEG,(E)> '||SQLERRM);
            END;

            UPDATE  MOD_BIEN_CERT MB
                SET MB.StsModBien   = 'ACT'
            WHERE   MB.IdeBien      = BIE.IdeBien
            AND     MB.NumMod       = nNumModBien;

            UPDATE  BIEN_CERT BC
                SET BC.StsBien      = 'ACT',
                    BC.FecExc       = NULL,
                    BC.CodMotvExc   = NULL,
                    BC.TextMotvExc  = NULL
            WHERE   BC.IdeBien = BIE.IdeBien;

        END LOOP;

        --|| CERTIFICADOS ||--
        FOR COC IN C_COBERT_CERT ( RAM.CodPlan,RAM.RevPlan,RAM.CodRamoCert ) LOOP

            UPDATE  COBERT_CERT CC
                SET CC.StsCobert    = 'ACT',
                    CC.FecExc       = NULL,
                    CC.CodMotvExc   = NULL,
                    CC.TextMotvExc  = NULL
            WHERE   CC.IdeCobert = COC.IdeCobert;

            UPDATE MOD_COBERT MC SET MC.StsModCobert = 'ACT' WHERE MC.IdeCobert = COC.IdeCobert;
            PR_COBERT_CERT.ACT_MOD (COC.IdeCobert,1);

        END LOOP;

        UPDATE CERT_RAMO CR
        SET    CR.FecExc          = NULL,
               CR.CodMotvExc      = NULL,
               CR.TextMotvExc     = NULL,
               CR.StsCertRamo     = 'ACT'
        WHERE  CR.IdePol      = RAM.IdePol
        AND    CR.NumCert     = RAM.NumCert
        AND    CR.CodPlan     = RAM.CodPlan
        AND    CR.RevPlan     = RAM.RevPlan
        AND    CR.CodRamoCert = RAM.CodRamoCert;

    END LOOP;

    FOR CER IN C_CERT LOOP

        UPDATE CERTIFICADO CE
        SET    CE.FecExc      = NULL,
               CE.CodMotvExc  = NULL,
               CE.TextMotvExc = NULL,
               CE.IndExcluir  = NULL,
               CE.StsCert     = 'ACT'
        WHERE  CE.IdePol  = CER.IdePol
        AND    CE.NumCert = CER.NumCert;

        UPDATE MOD_DIREC_RIESGO_CERT DI
        SET    DI.StsDir    = 'ACT'
        WHERE  DI.IdePol  = CER.IdePol
        AND    DI.NumCert = CER.NumCert;

    END LOOP;

    UPDATE  POLIZA PO
        SET PO.StsPol           = 'ACT',
            PO.FecAnul          = NULL,
            PO.ClaseAnul        = NULL,
            PO.CodMotvAnul      = NULL,
            PO.SubCodMotvAnul   = NULL,
            PO.TextMotvAnul     = NULL
    WHERE   PO.IdePol    = pIdePol
    AND     PO.StsPol    = 'ANU';

    UPDATE  DIST_COA DC
        SET DC.StsCoa       = 'ACT'
    WHERE   DC.IdePol   = pIdePol
    AND     DC.StsCoa   = 'ANU';

    UPDATE  MOD_DIST_COA MD
        SET MD.StsModCoa     = 'ACT'
    WHERE   MD.IdePol     = pIdePol
    AND     MD.StsModCoa  = 'ANU';

    UPDATE OPER_POL OP SET OP.TipoOp = 'MOD' WHERE OP.IdePol = pIdePol AND OP.TipoOp = 'ANU' AND OP.MtoOper = 0;  --COLWSSEGUR-6414--

END;

PROCEDURE   EJEC_PROC_REHAB ( pIdeProceso NUMBER,pIdJob NUMBER,pCodigo VARCHAR2,pIdCarga NUMBER,pFechaIni DATE,pFechaFin DATE ) IS

cRowREHAB           PROC_REHAB_POLIZA%ROWTYPE  := NULL;

cCodigo             VARCHAR2(500);
cMensaje            VARCHAR2(500);
cBackTrace          VARCHAR2(500);

nNumOperEMI         OPER_POL.NumOper%TYPE;
nNumOperANU         OPER_POL.NumOper%TYPE;
cExOperMOD          VARCHAR2(1);
nIdePol             NUMBER(14);
nNumCert            NUMBER(14);
cIndRecaWeb         VARCHAR2(50);
cIndOblig           VARCHAR2(50);
cIndCompensa        VARCHAR2(50);
cIndRehabilita      VARCHAR2(50);
cIndModCobert       VARCHAR2(50);
nLimit              NUMBER(10);

VAL_POL             EXCEPTION;

CURSOR C_POLIZA IS
    SELECT  PP.*
    FROM    T$_PROC_REHAB_POLIZA PP
    WHERE   PP.FecSts   = pFechaFin;

TYPE  CARGA_POL IS TABLE OF C_POLIZA%ROWTYPE INDEX BY PLS_INTEGER;
C_POL CARGA_POL;

BEGIN

    cIndRecaWeb     := NVL(PR_PROG_INTERFACE.BUSCA_VAL_PARAM_NO_1 ( pIdJob,'INDRECAWEB' ),'S');
    cIndCompensa    := NVL(PR_PROG_INTERFACE.BUSCA_VAL_PARAM_NO_1 ( pIdJob,'INDCOMPENSA' ),'S');
    cIndOblig       := NVL(PR_PROG_INTERFACE.BUSCA_VAL_PARAM_NO_1 ( pIdJob,'INDOBLIG' ),'S');
    cIndRehabilita  := NVL(PR_PROG_INTERFACE.BUSCA_VAL_PARAM_NO_1 ( pIdJob,'INDREHABILITA' ),'N');
    cIndModCobert   := NVL(PR_PROG_INTERFACE.BUSCA_VAL_PARAM_NO_1 ( pIdJob,'INDMODCOBERT' ),'N');

    BEGIN
        nLimit          := TO_NUMBER(PR_PROG_INTERFACE.BUSCA_VAL_PARAM_NO_1 ( pIdJob,'BULKCOLLECT' ));
    EXCEPTION
        WHEN OTHERS THEN
            nLimit  := 100;
    END;

    OPEN C_POLIZA;
    LOOP
        FETCH C_POLIZA BULK COLLECT
            INTO C_POL LIMIT 100;
        EXIT WHEN C_POL.COUNT = 0;
        FOR x IN 1 .. C_POL.COUNT LOOP

        BEGIN

            cCodigo         := NULL;
            cMensaje        := NULL;
            cBackTrace      := NULL;

            cRowREHAB                   := NULL;
            cRowREHAB.IdeProceso        := pIdeProceso;
            cRowREHAB.FecSts            := TRUNC(SYSDATE);
            cRowREHAB.IdePol            := C_POL(x).IdePol;
            cRowREHAB.NumCert           := C_POL(x).NumCert;
            cRowREHAB.IndRecaWeb        := cIndRecaWeb;
            cRowREHAB.IndOblig          := cIndOblig;
            cRowREHAB.IndCompensa       := cIndCompensa;
            cRowREHAB.IndRehabilita     := cIndRehabilita;
            cRowREHAB.IndModCobert      := cIndModCobert;

            nIdePol     := C_POL(x).IdePol;
            nNumCert    := NVL(C_POL(x).NumCert,1);

            BEGIN
                SELECT  MAX(CASE WHEN OP.TipoOp = 'EMI' THEN OP.NumOper ELSE 0 END) OPEREMI,
                        MAX(CASE WHEN OP.TipoOp = 'ANU' THEN OP.NumOper ELSE 0 END) OPERANU
                INTO    nNumOperEMI,nNumOperANU
                FROM    OPER_POL OP
                WHERE   OP.IdePol   = C_POL(x).IdePol
                AND     OP.NumCert  = C_POL(x).NumCert
                GROUP BY OP.IdePol,OP.NumCert;
            EXCEPTION
                WHEN OTHERS THEN
                    nNumOperEMI     := NULL;
                    nNumOperANU     := NULL;
                    RAISE_APPLICATION_ERROR (-20100,'ERROR: ValidarOperPol,(E)> '||SQLERRM);
            END;

            IF nNumOperEMI = 0 OR nNumOperANU = 0 THEN
                cCodigo     := '901';
                cMensaje    := 'ERROR: Poliza sin Operación de Emisión y/o Anulación Definidas. ';
                RAISE VAL_POL;
            END IF;

            BEGIN
                SELECT  'S'
                INTO    cExOperMOD
                FROM    OPER_POL OP
                WHERE   OP.IdePol   = C_POL(x).IdePol
                AND     OP.NumCert  = C_POL(x).NumCert
                AND     OP.NumOper > nNumOperEMI
                AND     OP.NumOper < nNumOperANU
                AND     ROWNUM     = 1;
            EXCEPTION
                WHEN NO_DATA_FOUND THEN
                    cExOperMOD      := 'N';
                WHEN OTHERS THEN
                    cExOperMOD      := NULL;
                    RAISE_APPLICATION_ERROR (-20100,'ERROR: ValidarOperPol,(E)> '||SQLERRM);
            END;

            IF cExOperMOD = 'S' THEN
                cCodigo     := '902';
                cMensaje    := 'ERROR: Poliza con Suplementos Antes de la Anulación.';
                RAISE VAL_POL;
            END IF;

            SAVEPOINT SP_POLIZA;

            PR_POLIZA.CARGAR ( nIdePol );

            IF PR_POLIZA.cStsPol != 'ANU' THEN
                cCodigo     := '902';
                cMensaje    := 'ERROR: La Poliza debe estar Anulada para Continuar';
                RAISE VAL_POL;
            END IF;

            IF cIndRecaWeb = 'S' THEN
                PR_INT_PROC_MANUAL.REVERTIR_RECAUDO_POLWEB ( nIdePol,nNumCert,cIndModCobert );
            END IF;

            IF cIndCompensa = 'S' THEN
                PR_INT_PROC_MANUAL.REVERTIR_COMPENSACION ( nIdePol,nNumCert );
            END IF;

            IF cIndOblig = 'S' THEN
                PR_INT_PROC_MANUAL.REVERTIR_OBLIG_DEVOLUCION ( nIdePol,nNumCert,cIndModCobert );
            END IF;

            IF cIndRehabilita = 'S' THEN
                PR_INT_PROC_MANUAL.REHABILITAR_POL_SIN_OPERPOL ( nIdePol );
            END IF;

            cRowREHAB.Estado        := 'OK';
            cRowREHAB.Resultado     := NULL;

            PR_INT_PROC_MANUAL.INSERT_PROC_REHAB_POLIZA ( cRowREHAB );

        EXCEPTION
            WHEN VAL_POL THEN

                cBackTrace  := DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;

                PR_INT_FUNCIONES.INSERTAR_LOG
                                 ( pIdeProceso   =>  pIdeProceso,
                                   pStsLog       =>  'ERR',
                                   pInterfaz     =>  'EJEC_PROC_REHAB',
                                   pCodigo       =>  '999',
                                   pDescripcion  =>  cMensaje,
                                   pNombLlave    =>  'IDEPOL',
                                   pValorLlave   =>  nIdePol,
                                   pNombIdenGen  =>  'POLIZA',
                                   pIdenGen      =>  PR_POLIZA.cCodProd||'-'||PR_POLIZA.nNumPol,
                                   pNumLote      =>  1,
                                   pOraError     =>  cBackTrace,
                                   pIndLogEmi    => 'S',
                                   pNumLinea     =>  1
                                 );

                cRowREHAB.Estado        := 'ERR';
                cRowREHAB.Resultado     := cMensaje;
                PR_INT_PROC_MANUAL.INSERT_PROC_REHAB_POLIZA ( cRowREHAB );
                ROLLBACK TO SP_POLIZA;

            WHEN OTHERS THEN

                cCodigo     := SQLCODE;
                cMensaje    := SQLERRM;
                cBackTrace  := DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;

                PR_INT_FUNCIONES.INSERTAR_LOG
                                 ( pIdeProceso   =>  pIdeProceso,
                                   pStsLog       =>  'ERR',
                                   pInterfaz     =>  'EJEC_PROC_REHAB',
                                   pCodigo       =>  '999',
                                   pDescripcion  =>  cMensaje,
                                   pNombLlave    =>  'IDEPOL',
                                   pValorLlave   =>  nIdePol,
                                   pNombIdenGen  =>  'POLIZA',
                                   pIdenGen      =>  PR_POLIZA.cCodProd||'-'||PR_POLIZA.nNumPol,
                                   pNumLote      =>  1,
                                   pOraError     =>  cBackTrace,
                                   pIndLogEmi    => 'S',
                                   pNumLinea     =>  1
                                 );

                cRowREHAB.Estado        := 'ERR';
                cRowREHAB.Resultado     := cMensaje;
                PR_INT_PROC_MANUAL.INSERT_PROC_REHAB_POLIZA ( cRowREHAB );
                ROLLBACK TO SP_POLIZA;
        END;

        COMMIT;
        END LOOP;
    END LOOP;

END;

PROCEDURE   INSERT_PROC_REHAB_POLIZA ( pRowREHAB PROC_REHAB_POLIZA%ROWTYPE ) IS

PRAGMA      AUTONOMOUS_TRANSACTION;

BEGIN

    BEGIN
        INSERT INTO PROC_REHAB_POLIZA VALUES pRowREHAB;
    EXCEPTION
        WHEN OTHERS THEN
            RAISE_APPLICATION_ERROR(-20100,'ERROR: InsertRehabPol,(E)> '||SQLERRM);
    END;
    COMMIT;

END;

END;
/
