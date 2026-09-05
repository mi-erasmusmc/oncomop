# Documentation of TNM staging rules

Internal auxiliary files used to determine cancer stage.

## Source

UICC_7th edition.pdf; UICC_8th edition.pdf corroborated by nhs/\*.pdf;
uicc/UICC_9th edition.pdf

## Details

The package stores three rule sets derived from UICC guidelines for the
TNM staging system. The rules stored in these files are differentiated
by cancer **site** (bladder, breast, colorectal, lung, oesophageal,
prostate, skin), classification **type** (clinical, pathological, base)
and classification **edition** (7th, 8th, unspecified). The files are:

1.  `tnm_concepts.csv`: contains information about the individual TNM
    components and their concept ids, differentiated by type and
    edition. There are \\393\\ total concepts for \\44\\ unique
    components:

        TX, T0, Tis, Ta, T1, T1a, T1b, T1c, T1mi, T2, T2a, T2b, T2c, T3, T3a,
        T3b, T4, T4a, T4b, T4c, T4d, NX, N0, N1, N1a, N1b, N1c, N1mi, N2, N2a,
        N2b, N2c, N3, N3a, N3b, N3c, M0, M1, M1a, M1b, M1c, M1c1, M1c2, M1d.

    These components have different versions according to:

    - Edition: \\131\\ concepts for 7th, \\131\\ for 8th, \\131\\ for
      unspecified;

    - Type: \\132\\ concepts for base, \\132\\ for clinical, \\129\\ for
      pathological (the `M0` component is not valid in the pathological
      setting, for any of the editions).

2.  `tnm_stage_mapping.csv`: contains the rules to determine the cancer
    stage based on a combination of individual TNM components,
    differentiated by cancer site, type (clinical, pathological, base)
    and by edition (7th, 8th, 9th). Each rule refers to a specific
    source page of the UICC guidelines.

    The currently available rules support the following concept
    categories.

    For `bladder` cancer:

    - Edition: 7th, 8th, 9th available;

    - Type: only "base" is available, for all editions.

    For `breast` cancer:

    - Edition: 7th, 8th, 9th available;

    - Type: only "base" is available, for all editions.

    For `colorectal` cancer:

    - Edition: 7th, 8th, 9th available;

    - Type: only "base" is available, for all editions.

    For `lung` cancer:

    - Edition: 7th, 8th, 9th available;

    - Type: only "base" is available, for all editions.

    For `oesophageal` cancer:

    - For 7th edition, only "base" type is available;

    - For 8th edition, only "clinical" and "pathological" types are
      available but not "base".

    - For 9th edition, only "clinical" and "pathological" types are
      available but not "base".

    For `prostate` cancer:

    - For 7th edition, only "base" type is available;

    - For 8th edition, only "clinical" type is available;

    - For 9th edition, only "clinical" and "pathological" types are
      available but not "base".

    For `skin` cancer:

    - For 7th edition, only "pathological" type is available;

    - For 8th edition, only "clinical" and "pathological" types are
      available but not "base".

    - For 9th edition, only "clinical" and "pathological" types are
      available but not "base".

3.  `tnm_stage_shortcut_mapping.csv`: contains some more general rules
    to determine the cancer stage based on a subset of individual TNM
    components, differentiated by cancer site, type and edition. In
    fact, there are some special cases in which the value of one or two
    components is enough to determine the stage, independently of the
    others. Each rule refers to a specific source page of the UICC
    guidelines.

These files are meant for internal use and are not intended to be
modified by users.
