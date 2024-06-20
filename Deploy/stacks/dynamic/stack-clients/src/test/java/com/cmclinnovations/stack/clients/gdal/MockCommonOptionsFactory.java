package com.cmclinnovations.stack.clients.gdal;

import javax.annotation.Nonnull;

final class MockCommonOptionsFactory extends AbstractOptionsFactory<MockCommonOptions> {

    public enum Args implements IArgs<MockCommonOptions> {
        trivial, trivialFile,
        inputDatasetOpenOptions, inputDatasetOpenOptionsFile,
        otherOptions, otherOptionsFile,
        otherOptionsArray, otherOptionsArrayFile,
        sridIn, sridInFile, sridOut, sridOutFile, sridBoth, sridBothFile;

        private static final @Nonnull MockCommonOptionsFactory FACTORY = new MockCommonOptionsFactory();

        @Override
        public MockCommonOptionsFactory getFactory() {
            return FACTORY;
        }

        @Override
        public String getCommand() {
            return "testCommand";
        }
    }

    MockCommonOptionsFactory() {
        super(MockCommonOptions.class);
    }

    @Override
    protected void configureOptions(IArgs<MockCommonOptions> optionEnum, MockCommonOptions options) {
        switch (Args.class.cast(optionEnum)) {
            case sridIn:
                options.setSridIn(AbstractOptionsFactory.SRID_IN);
                break;
            case sridBoth:
                options.setSridIn(AbstractOptionsFactory.SRID_IN);
            case sridOut:
                options.setSridOut(AbstractOptionsFactory.SRID_OUT);
                break;
            case inputDatasetOpenOptions:
                options.addInputDatasetOpenOption("X_POSSIBLE_NAMES", X_POSSIBLE_NAMES_VALUE);
                options.addInputDatasetOpenOption("Y_POSSIBLE_NAMES", Y_POSSIBLE_NAMES_VALUE);
                break;
            case otherOptions:
                options.addOtherOption("-select", OTHER_OPTIONS_SELECT_VALUE);
                break;
            case otherOptionsArray:
                options.addOtherOption("-spat", OTHER_OPTIONS_SELECT_VALUE.split(","));
                break;
            default:
                break;
        }
    }

}